module Wizard.Service.Questionnaire.QuestionnaireService where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Localization.Messages.Public
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Localization.Messages.Internal
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireAclHelpers
import Wizard.Service.Acl.AclService
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Mail.Mailer
import Wizard.Service.Package.PackageService
import Wizard.Service.Questionnaire.Collaboration.CollaborationService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireUtils
import Wizard.Service.Questionnaire.QuestionnaireValidation

getQuestionnairesForCurrentUserPageDto :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page QuestionnaireDTO)
getQuestionnairesForCurrentUserPageDto mQuery pageable sort = do
  checkPermission _QTN_PERM
  currentUser <- getCurrentUser
  qtnPage <- findQuestionnairesForCurrentUserPage mQuery pageable sort
  traverse enhanceQuestionnaire qtnPage

createQuestionnaire :: QuestionnaireCreateDTO -> AppContextM QuestionnaireDTO
createQuestionnaire questionnaireCreateDto =
  liftIO generateUuid >>= createQuestionnaireWithGivenUuid questionnaireCreateDto

createQuestionnaireWithGivenUuid :: QuestionnaireCreateDTO -> U.UUID -> AppContextM QuestionnaireDTO
createQuestionnaireWithGivenUuid reqDto qtnUuid = do
  checkPermission _QTN_PERM
  currentUser <- getCurrentUser
  package <- findPackageWithEventsById (reqDto ^. packageId)
  qtnState <- getQuestionnaireState (U.toString qtnUuid) (reqDto ^. packageId)
  now <- liftIO getCurrentTime
  visibility <- extractVisibility reqDto
  sharing <- extractSharing reqDto
  let qtn = fromQuestionnaireCreateDTO reqDto qtnUuid visibility sharing (currentUser ^. uuid) now now
  insertQuestionnaire qtn
  report <- getQuestionnaireReport qtn
  permissionDtos <- traverse enhanceQuestionnairePermRecord (qtn ^. permissions)
  qtnCtn <- compileQuestionnaire qtn
  return $ toSimpleDTO qtn qtnCtn package qtnState report permissionDtos

cloneQuestionnaire :: String -> AppContextM QuestionnaireDTO
cloneQuestionnaire cloneUuid = do
  checkPermission _QTN_PERM
  originQtn <- findQuestionnaireById cloneUuid
  checkViewPermissionToQtn (originQtn ^. visibility) (originQtn ^. sharing) (originQtn ^. permissions)
  pkg <- findPackageWithEventsById (originQtn ^. packageId)
  newUuid <- liftIO generateUuid
  currentUser <- getCurrentUser
  now <- liftIO getCurrentTime
  let ownerPerm = toUserPermRecord (currentUser ^. uuid) ownerPermissions
  let newPermissions = ownerPerm : removeUserPermission (currentUser ^. uuid) (originQtn ^. permissions)
  let newQtn =
        permissions .~ newPermissions $ uuid .~ newUuid $ name .~ ("Copy of " ++ originQtn ^. name) $ updatedAt .~ now $
        originQtn
  insertQuestionnaire newQtn
  state <- getQuestionnaireState (U.toString newUuid) (pkg ^. pId)
  report <- getQuestionnaireReport newQtn
  permissionDtos <- traverse enhanceQuestionnairePermRecord (newQtn ^. permissions)
  qtnCtn <- compileQuestionnaire newQtn
  return $ toSimpleDTO newQtn qtnCtn pkg state report permissionDtos

getQuestionnaireById :: String -> AppContextM QuestionnaireDTO
getQuestionnaireById qtnUuid = do
  mQtn <- getQuestionnaireById' qtnUuid
  case mQtn of
    Just qtn -> return qtn
    Nothing -> throwError $ NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "questionnaire" qtnUuid

getQuestionnaireById' :: String -> AppContextM (Maybe QuestionnaireDTO)
getQuestionnaireById' qtnUuid = do
  mQtn <- findQuestionnaireById' qtnUuid
  case mQtn of
    Just qtn -> do
      checkViewPermissionToQtn (qtn ^. visibility) (qtn ^. sharing) (qtn ^. permissions)
      package <- getPackageById (qtn ^. packageId)
      state <- getQuestionnaireState qtnUuid (package ^. pId)
      report <- getQuestionnaireReport qtn
      permissionDtos <- traverse enhanceQuestionnairePermRecord (qtn ^. permissions)
      qtnCtn <- compileQuestionnaire qtn
      return . Just $ toDTO qtn qtnCtn package state report permissionDtos
    Nothing -> return Nothing

getQuestionnaireDetailById :: String -> AppContextM QuestionnaireDetailDTO
getQuestionnaireDetailById qtnUuid = do
  qtn <- findQuestionnaireById qtnUuid
  checkViewPermissionToQtn (qtn ^. visibility) (qtn ^. sharing) (qtn ^. permissions)
  pkg <- getPackageById (qtn ^. packageId)
  pkgVersions <- getPackageVersions pkg
  knowledgeModel <- compileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids)
  state <- getQuestionnaireState qtnUuid (qtn ^. packageId)
  mTemplate <-
    case qtn ^. templateId of
      Just tId -> do
        template <- findTemplateById tId
        return $ Just template
      _ -> return Nothing
  mFormat <-
    case (mTemplate, qtn ^. formatUuid) of
      (Just template, Just fUuid) -> return $ L.find (\f -> f ^. uuid == fUuid) (template ^. formats)
      _ -> return Nothing
  permissionDtos <- traverse enhanceQuestionnairePermRecord (qtn ^. permissions)
  qtnCtn <- compileQuestionnaire qtn
  eventsDto <- traverse enhanceQuestionnaireEvent (qtn ^. events)
  versionDto <- traverse enhanceQuestionnaireVersion (qtn ^. versions)
  return $
    toDetailWithPackageWithEventsDTO
      qtn
      qtnCtn
      pkg
      pkgVersions
      knowledgeModel
      state
      mTemplate
      mFormat
      (qtnCtn ^. replies)
      permissionDtos
      eventsDto
      versionDto

modifyQuestionnaire :: String -> QuestionnaireChangeDTO -> AppContextM QuestionnaireDetailDTO
modifyQuestionnaire qtnUuid reqDto = do
  checkPermission _QTN_PERM
  qtn <- findQuestionnaireById qtnUuid
  qtnDto <- getQuestionnaireDetailById qtnUuid
  checkOwnerPermissionToQtn (qtn ^. visibility) (qtn ^. permissions)
  currentUser <- getCurrentUser
  now <- liftIO getCurrentTime
  qVisibility <- extractVisibility reqDto
  qSharing <- extractSharing reqDto
  let updatedQtn = fromChangeDTO qtn reqDto qVisibility qSharing (currentUser ^. uuid) now
  let pkgId = qtnDto ^. package . pId
  updateQuestionnaireById updatedQtn
  knowledgeModel <- compileKnowledgeModel [] (Just pkgId) (updatedQtn ^. selectedTagUuids)
  state <- getQuestionnaireState qtnUuid pkgId
  updatePermsForOnlineUsers qtnUuid (updatedQtn ^. visibility) (updatedQtn ^. sharing) (updatedQtn ^. permissions)
  permissionDtos <- traverse enhanceQuestionnairePermRecord (updatedQtn ^. permissions)
  catchError
    (sendQuestionnaireInvitationMail qtn updatedQtn)
    (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_QTN__INVITATION_EMAIL_NOT_SENT)
  qtnCtn <- compileQuestionnaire updatedQtn
  eventsDto <- traverse enhanceQuestionnaireEvent (updatedQtn ^. events)
  versionDto <- traverse enhanceQuestionnaireVersion (qtn ^. versions)
  return $
    toDetailWithPackageDTO
      updatedQtn
      qtnCtn
      (qtnDto ^. package)
      knowledgeModel
      state
      Nothing
      Nothing
      (qtnCtn ^. replies)
      permissionDtos
      eventsDto
      versionDto

deleteQuestionnaire :: String -> AppContextM ()
deleteQuestionnaire qtnUuid = do
  checkPermission _QTN_PERM
  qtn <- findQuestionnaireById qtnUuid
  validateQuestionnaireDeletation qtnUuid
  checkOwnerPermissionToQtn (qtn ^. visibility) (qtn ^. permissions)
  deleteMigratorStateByNewQuestionnaireId qtnUuid
  documents <- findDocumentsFiltered [("questionnaireUuid", qtnUuid)]
  traverse_
    (\d -> do
       deleteDocumentsFiltered [("uuid", U.toString $ d ^. uuid)]
       deleteDocumentContentsFiltered [("filename", U.toString $ d ^. uuid)])
    documents
  deleteQuestionnaireById qtnUuid
  logOutOnlineUsersWhenQtnDramaticallyChanged qtnUuid
  return ()

removeOwnerFromQuestionnaire :: U.UUID -> AppContextM ()
removeOwnerFromQuestionnaire userUuid = do
  qtns <- findQuestionnairesOwnedByUser (U.toString userUuid)
  traverse_ processQtn qtns
  where
    processQtn :: Questionnaire -> AppContextM ()
    processQtn qtn = do
      let newPermissions = removeUserPermission userUuid (qtn ^. permissions)
      if null newPermissions
        then deleteQuestionnaire (U.toString $ qtn ^. uuid)
        else do
          let reqDto = toChangeDTO qtn & permissions .~ newPermissions
          modifyQuestionnaire (U.toString $ qtn ^. uuid) reqDto
          return ()

modifyContent :: String -> QuestionnaireContentChangeDTO -> AppContextM QuestionnaireContentChangeDTO
modifyContent qtnUuid reqDto = do
  qtn <- findQuestionnaireById qtnUuid
  checkEditPermissionToQtn (qtn ^. visibility) (qtn ^. sharing) (qtn ^. permissions)
  currentUser <- getCurrentUser
  now <- liftIO getCurrentTime
  let updatedQtn = fromContentChangeDTO qtn reqDto currentUser now
  updateQuestionnaireById updatedQtn
  return reqDto
