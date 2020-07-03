module Wizard.Service.Questionnaire.QuestionnaireService where

import Control.Lens ((.~), (^.), (^?), _Just)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.DAO.Package.PackageDAO
import Shared.Localization.Messages.Public
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Error.Error
import Shared.Util.Uuid
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Common.ACL
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Questionnaire.QuestionnaireACL
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireUtils
import Wizard.Service.Questionnaire.QuestionnaireValidation
import Wizard.Service.User.UserService

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
  let qtn = fromQuestionnaireCreateDTO reqDto qtnUuid visibility (currentUser ^. uuid) now now
  insertQuestionnaire qtn
  let mOwner =
        case qtn ^. ownerUuid of
          Just uUuid -> Just currentUser
          Nothing -> Nothing
  report <- getQuestionnaireReport qtn
  return $ toSimpleDTO qtn package qtnState mOwner report

cloneQuestionnaire :: String -> AppContextM QuestionnaireDTO
cloneQuestionnaire cloneUuid = do
  checkPermission _QTN_PERM
  qtnDto <- getQuestionnaireDetailById cloneUuid
  pkg <- findPackageWithEventsById (qtnDto ^. package . pId)
  newUuid <- liftIO generateUuid
  currentUser <- getCurrentUser
  let newOwnerUuid =
        if qtnDto ^. visibility == PublicQuestionnaire
          then Nothing
          else Just $ currentUser ^. uuid
  now <- liftIO getCurrentTime
  let originQtn = fromDetailDTO qtnDto
  let newQtn =
        ownerUuid .~ newOwnerUuid $ uuid .~ newUuid $ name .~ ("Copy of " ++ originQtn ^. name) $ updatedAt .~ now $
        originQtn
  insertQuestionnaire newQtn
  state <- getQuestionnaireState (U.toString newUuid) (pkg ^. pId)
  report <- getQuestionnaireReport newQtn
  let mOwner =
        case newOwnerUuid of
          Just uUuid -> Just currentUser
          Nothing -> Nothing
  return $ toSimpleDTO newQtn pkg state mOwner report

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
      checkPermissionToQtn (qtn ^. visibility) (qtn ^. ownerUuid)
      package <- findPackageById (qtn ^. packageId)
      state <- getQuestionnaireState qtnUuid (package ^. pId)
      report <- getQuestionnaireReport qtn
      mOwner <-
        case qtn ^. ownerUuid of
          Just uUuid -> Just <$> getUserById (U.toString uUuid)
          Nothing -> return Nothing
      return . Just $ toDTO qtn package state mOwner report
    Nothing -> return Nothing

getQuestionnaireDetailById :: String -> AppContextM QuestionnaireDetailDTO
getQuestionnaireDetailById qtnUuid = do
  checkPermission _QTN_PERM
  qtn <- findQuestionnaireById qtnUuid
  checkPermissionToQtn (qtn ^. visibility) (qtn ^. ownerUuid)
  package <- findPackageWithEventsById (qtn ^. packageId)
  knowledgeModel <- compileKnowledgeModel [] (Just $ qtn ^. packageId) (qtn ^. selectedTagUuids)
  state <- getQuestionnaireState qtnUuid (package ^. pId)
  report <- getQuestionnaireReport qtn
  return $ toDetailWithPackageWithEventsDTO qtn package knowledgeModel state report

modifyQuestionnaire :: String -> QuestionnaireChangeDTO -> AppContextM QuestionnaireDetailDTO
modifyQuestionnaire qtnUuid reqDto = do
  checkPermission _QTN_PERM
  qtnDto <- getQuestionnaireDetailById qtnUuid
  checkEditPermissionToQtn (qtnDto ^. visibility) (qtnDto ^. ownerUuid)
  currentUser <- getCurrentUser
  now <- liftIO getCurrentTime
  visibility <- extractVisibility reqDto
  let updatedQtn = fromChangeDTO qtnDto reqDto visibility (currentUser ^. uuid) now
  let pkgId = qtnDto ^. package . pId
  updateQuestionnaireById updatedQtn
  knowledgeModel <- compileKnowledgeModel [] (Just pkgId) (updatedQtn ^. selectedTagUuids)
  state <- getQuestionnaireState qtnUuid pkgId
  report <- getQuestionnaireReport updatedQtn
  return $ toDetailWithPackageDTO updatedQtn (qtnDto ^. package) knowledgeModel state report

deleteQuestionnaire :: String -> AppContextM ()
deleteQuestionnaire qtnUuid = do
  checkPermission _QTN_PERM
  qtn <- getQuestionnaireById qtnUuid
  validateQuestionnaireDeletation qtnUuid
  checkEditPermissionToQtn (qtn ^. visibility) (qtn ^? owner . _Just . uuid)
  deleteQuestionnaireById qtnUuid
  deleteMigratorStateByNewQuestionnaireId qtnUuid
  return ()
