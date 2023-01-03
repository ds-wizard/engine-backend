module Wizard.Service.Questionnaire.QuestionnaireService where

import Control.Monad (when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Localization.Messages.Public
import Shared.Model.Common.Lens
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Model.Template.Template
import Shared.Service.Package.PackageUtil
import Shared.Util.List
import Shared.Util.Uuid
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Submission.SubmissionDAO
import Wizard.Localization.Messages.Internal
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.ContextResult
import Wizard.Model.Document.Document
import Wizard.Model.Migration.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Questionnaire.QuestionnaireAclHelpers
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.S3.Document.DocumentS3
import Wizard.Service.Acl.AclService
import Wizard.Service.Config.App.AppConfigService
import Wizard.Service.Context.ContextService
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Limit.AppLimitService
import Wizard.Service.Mail.Mailer
import Wizard.Service.Package.PackageService
import Wizard.Service.Questionnaire.Collaboration.CollaborationService
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireAudit
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireUtil
import Wizard.Service.Questionnaire.QuestionnaireValidation
import Wizard.Util.Logger

getQuestionnairesForCurrentUserPageDto
  :: Maybe String
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe [String]
  -> Maybe String
  -> Maybe [String]
  -> Maybe String
  -> Maybe [String]
  -> Maybe String
  -> Pageable
  -> [Sort]
  -> AppContextM (Page QuestionnaireDTO)
getQuestionnairesForCurrentUserPageDto mQuery mIsTemplate mIsMigrating mProjectTags mProjectTagsOp mUserUuids mUserUuidsOp mPackageIds mPackageIdsOp pageable sort = do
  checkPermission _QTN_PERM
  currentUser <- getCurrentUser
  qtnPage <-
    findQuestionnairesForCurrentUserPage
      mQuery
      mIsTemplate
      mIsMigrating
      mProjectTags
      mProjectTagsOp
      mUserUuids
      mUserUuidsOp
      mPackageIds
      mPackageIdsOp
      pageable
      sort
  return . fmap toDTO' $ qtnPage

createQuestionnaire :: QuestionnaireCreateDTO -> AppContextM QuestionnaireDTO
createQuestionnaire questionnaireCreateDto =
  liftIO generateUuid >>= createQuestionnaireWithGivenUuid questionnaireCreateDto

createQuestionnaireWithGivenUuid :: QuestionnaireCreateDTO -> U.UUID -> AppContextM QuestionnaireDTO
createQuestionnaireWithGivenUuid reqDto qtnUuid =
  runInTransaction $ do
    checkQuestionnaireLimit
    checkCreatePermissionToQtn
    pkgId <- resolvePackageId reqDto.packageId
    package <- findPackageWithEventsById pkgId
    qtnState <- getQuestionnaireState (U.toString qtnUuid) pkgId
    now <- liftIO getCurrentTime
    appUuid <- asks currentAppUuid
    visibility <- extractVisibility reqDto
    sharing <- extractSharing reqDto
    qtnPermUuid <- liftIO generateUuid
    mCurrentUser <- asks currentUser
    knowledgeModel <- compileKnowledgeModel [] (Just pkgId) reqDto.questionTagUuids
    phaseEventUuid <- liftIO generateUuid
    let qtn =
          fromQuestionnaireCreateDTO
            reqDto
            qtnUuid
            visibility
            sharing
            (fmap (.uuid) mCurrentUser)
            pkgId
            phaseEventUuid
            (headSafe knowledgeModel.phaseUuids)
            appUuid
            now
            qtnPermUuid
    insertQuestionnaire qtn
    recomputeQuestionnaireIndication qtn.uuid
    permissionDtos <- traverse enhanceQuestionnairePermRecord qtn.permissions
    qtnCtn <- compileQuestionnaire qtn
    return $ toSimpleDTO qtn package qtnState permissionDtos

createQuestionnaireFromTemplate :: QuestionnaireCreateFromTemplateDTO -> AppContextM QuestionnaireDTO
createQuestionnaireFromTemplate reqDto =
  runInTransaction $ do
    checkQuestionnaireLimit
    originQtn <- findQuestionnaireById (U.toString reqDto.questionnaireUuid)
    checkCreateFromTemplatePermissionToQtn originQtn.isTemplate
    pkg <- findPackageWithEventsById originQtn.packageId
    newUuid <- liftIO generateUuid
    qtnPermUuid <- liftIO generateUuid
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    appConfig <- getAppConfig
    let newVisibility = appConfig.questionnaire.questionnaireVisibility.defaultValue
    let newSharing = appConfig.questionnaire.questionnaireSharing.defaultValue
    let newPermissions = [toUserPermRecord qtnPermUuid newUuid currentUser.uuid ownerPermissions]
    let newQtn =
          originQtn
            { uuid = newUuid
            , name = reqDto.name
            , description = Nothing
            , sharing = newSharing
            , visibility = newVisibility
            , permissions = newPermissions
            , isTemplate = False
            , creatorUuid = Just $ currentUser.uuid
            , createdAt = now
            , updatedAt = now
            }
          :: Questionnaire
    insertQuestionnaire newQtn
    recomputeQuestionnaireIndication newQtn.uuid
    duplicateCommentThreads (U.toString reqDto.questionnaireUuid) newUuid
    state <- getQuestionnaireState (U.toString newUuid) pkg.pId
    permissionDtos <- traverse enhanceQuestionnairePermRecord newQtn.permissions
    qtnCtn <- compileQuestionnaire newQtn
    return $ toSimpleDTO newQtn pkg state permissionDtos

cloneQuestionnaire :: String -> AppContextM QuestionnaireDTO
cloneQuestionnaire cloneUuid =
  runInTransaction $ do
    checkQuestionnaireLimit
    originQtn <- findQuestionnaireById cloneUuid
    checkClonePermissionToQtn originQtn.visibility originQtn.sharing originQtn.permissions
    pkg <- findPackageWithEventsById originQtn.packageId
    newUuid <- liftIO generateUuid
    qtnPermUuid <- liftIO generateUuid
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    let ownerPerm = toUserPermRecord newUuid qtnPermUuid currentUser.uuid ownerPermissions
    let newPermissions = ownerPerm : removeUserPermission currentUser.uuid originQtn.permissions
    newDuplicatedPermissions <- traverse (duplicateUserPermission newUuid) newPermissions
    let newQtn =
          originQtn
            { uuid = newUuid
            , name = "Copy of " ++ originQtn.name
            , permissions = newDuplicatedPermissions
            , updatedAt = now
            }
          :: Questionnaire
    insertQuestionnaire newQtn
    duplicateCommentThreads cloneUuid newUuid
    recomputeQuestionnaireIndication newQtn.uuid
    state <- getQuestionnaireState (U.toString newUuid) pkg.pId
    permissionDtos <- traverse enhanceQuestionnairePermRecord newQtn.permissions
    return $ toSimpleDTO newQtn pkg state permissionDtos

getQuestionnaireById :: String -> AppContextM QuestionnaireDTO
getQuestionnaireById qtnUuid = do
  mQtn <- getQuestionnaireById' qtnUuid
  case mQtn of
    Just qtn -> return qtn
    Nothing -> throwError $ NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "questionnaire" [("uuid", qtnUuid)]

getQuestionnaireById' :: String -> AppContextM (Maybe QuestionnaireDTO)
getQuestionnaireById' qtnUuid = do
  mQtn <- findQuestionnaireById' qtnUuid
  case mQtn of
    Just qtn -> do
      checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
      package <- getPackageById qtn.packageId
      state <- getQuestionnaireState qtnUuid package.pId
      permissionDtos <- traverse enhanceQuestionnairePermRecord qtn.permissions
      return . Just $ toDTO qtn package state permissionDtos
    Nothing -> return Nothing

getQuestionnaireDetailById :: String -> AppContextM QuestionnaireDetailDTO
getQuestionnaireDetailById qtnUuid = do
  qtn <- findQuestionnaireById qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  pkg <- getPackageById qtn.packageId
  pkgVersions <- getPackageVersions pkg
  knowledgeModel <- compileKnowledgeModel [] (Just qtn.packageId) qtn.selectedQuestionTagUuids
  state <- getQuestionnaireState qtnUuid qtn.packageId
  mTemplate <-
    case qtn.templateId of
      Just tId -> do
        template <- findTemplateById tId
        return $ Just template
      _ -> return Nothing
  mFormat <-
    case (mTemplate, qtn.formatUuid) of
      (Just template, Just fUuid) -> return $ L.find (\f -> f.uuid == fUuid) template.formats
      _ -> return Nothing
  permissionDtos <- traverse enhanceQuestionnairePermRecord qtn.permissions
  qtnCtn <- compileQuestionnaire qtn
  versionDto <- traverse enhanceQuestionnaireVersion qtn.versions
  commentThreadsMap <- getQuestionnaireComments qtn
  migrations <- findMigratorStatesByOldQuestionnaireId qtnUuid
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
      qtnCtn.replies
      commentThreadsMap
      permissionDtos
      versionDto
      (fmap (.newQuestionnaireUuid) . headSafe $ migrations)

getQuestionnaireEventsForQtnUuid :: String -> AppContextM [QuestionnaireEventDTO]
getQuestionnaireEventsForQtnUuid qtnUuid = do
  qtn <- findQuestionnaireById qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  auditQuestionnaireListEvents qtnUuid
  traverse enhanceQuestionnaireEvent qtn.events

getQuestionnaireEventForQtnUuid :: String -> String -> AppContextM QuestionnaireEventDTO
getQuestionnaireEventForQtnUuid qtnUuid eventUuid = do
  qtn <- findQuestionnaireById qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  auditQuestionnaireDetailEvent qtnUuid
  case L.find (\e -> U.toString (getUuid e) == eventUuid) qtn.events of
    Just event -> enhanceQuestionnaireEvent event
    Nothing ->
      throwError . NotExistsError $
        _ERROR_DATABASE__ENTITY_NOT_FOUND "questionnaire_event" [("questionnaireUuid", qtnUuid), ("eventUuid", eventUuid)]

modifyQuestionnaire :: String -> QuestionnaireChangeDTO -> AppContextM QuestionnaireDetailDTO
modifyQuestionnaire qtnUuid reqDto =
  runInTransaction $ do
    checkPermission _QTN_PERM
    validateQuestionnaireChangeDTO reqDto
    qtn <- findQuestionnaireById qtnUuid
    qtnDto <- getQuestionnaireDetailById qtnUuid
    skipIfAssigningProject qtn (checkOwnerPermissionToQtn qtn.visibility qtn.permissions)
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    qVisibility <- extractVisibility reqDto
    qSharing <- extractSharing reqDto
    let updatedQtn = fromChangeDTO qtn reqDto qVisibility qSharing currentUser now
    let pkgId = qtnDto.package.pId
    pkg <- getPackageById qtn.packageId
    pkgVersions <- getPackageVersions pkg
    updateQuestionnaireById updatedQtn
    knowledgeModel <- compileKnowledgeModel [] (Just pkgId) updatedQtn.selectedQuestionTagUuids
    state <- getQuestionnaireState qtnUuid pkgId
    updatePermsForOnlineUsers qtnUuid updatedQtn.visibility updatedQtn.sharing updatedQtn.permissions
    permissionDtos <- traverse enhanceQuestionnairePermRecord updatedQtn.permissions
    skipIfAssigningProject
      qtn
      ( catchError
          (sendQuestionnaireInvitationMail qtn updatedQtn)
          (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_QTN__INVITATION_EMAIL_NOT_SENT)
      )
    qtnCtn <- compileQuestionnaire updatedQtn
    versionDto <- traverse enhanceQuestionnaireVersion qtn.versions
    commentThreadsMap <- getQuestionnaireComments qtn
    deleteTemporalDocumentsByQuestionnaireUuid qtnUuid
    migrations <- findMigratorStatesByOldQuestionnaireId qtnUuid
    return $
      toDetailWithPackageWithEventsDTO
        updatedQtn
        qtnCtn
        pkg
        pkgVersions
        knowledgeModel
        state
        Nothing
        Nothing
        qtnCtn.replies
        commentThreadsMap
        permissionDtos
        versionDto
        Nothing

deleteQuestionnaire :: String -> Bool -> AppContextM ()
deleteQuestionnaire qtnUuid shouldValidatePermission =
  runInTransaction $ do
    qtn <- findQuestionnaireById qtnUuid
    validateQuestionnaireDeletation qtnUuid
    when shouldValidatePermission (checkOwnerPermissionToQtn qtn.visibility qtn.permissions)
    deleteMigratorStateByNewQuestionnaireId qtnUuid
    threads <- findQuestionnaireCommentThreads qtnUuid
    traverse_
      ( \t -> do
          deleteQuestionnaireCommentsByThreadUuid t.uuid
          deleteQuestionnaireCommentThreadById t.uuid
      )
      threads
    documents <- findDocumentsFiltered [("questionnaire_uuid", qtnUuid)]
    traverse_
      ( \d -> do
          deleteSubmissionsFiltered [("document_uuid", U.toString d.uuid)]
          deleteDocumentsFiltered [("uuid", U.toString d.uuid)]
          removeDocumentContent (U.toString d.uuid)
      )
      documents
    deleteQuestionnaireById qtnUuid
    logOutOnlineUsersWhenQtnDramaticallyChanged qtnUuid
    return ()

removeOwnerFromQuestionnaire :: U.UUID -> AppContextM ()
removeOwnerFromQuestionnaire userUuid =
  runInTransaction $ do
    qtns <- findQuestionnairesOwnedByUser (U.toString userUuid)
    traverse_ processQtn qtns
  where
    processQtn :: Questionnaire -> AppContextM ()
    processQtn qtn = do
      let newPermissions = removeUserPermission userUuid qtn.permissions
      if null newPermissions
        then deleteQuestionnaire (U.toString $ qtn.uuid) True
        else do
          let reqDto = toChangeDTO $ qtn {permissions = newPermissions}
          modifyQuestionnaire (U.toString $ qtn.uuid) reqDto
          return ()

modifyContent :: String -> QuestionnaireContentChangeDTO -> AppContextM QuestionnaireContentChangeDTO
modifyContent qtnUuid reqDto =
  runInTransaction $ do
    qtn <- findQuestionnaireById qtnUuid
    checkEditPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
    mCurrentUser <- asks currentUser
    now <- liftIO getCurrentTime
    let updatedQtn = fromContentChangeDTO qtn reqDto mCurrentUser now
    mPhasesAnsweredIndication <- getPhasesAnsweredIndication qtn
    case mPhasesAnsweredIndication of
      Just phasesAnsweredIndication ->
        updateQuestionnaireEventsWithIndicationByUuid qtnUuid False updatedQtn.events phasesAnsweredIndication
      Nothing -> updateQuestionnaireEventsByUuid qtnUuid False updatedQtn.events
    return reqDto

cleanQuestionnaires :: AppContextM ()
cleanQuestionnaires =
  runInTransaction $ do
    qtns <- findQuestionnaireWithZeroAcl
    traverse_
      ( \qtn -> do
          let qtnUuid = U.toString $ qtn.uuid
          logInfoU _CMP_SERVICE (f' "Clean questionnaire with empty ACL (qtnUuid: '%s')" [qtnUuid])
          deleteQuestionnaire qtnUuid False
      )
      qtns

recomputeQuestionnaireIndicationsInAllApplications :: AppContextM ()
recomputeQuestionnaireIndicationsInAllApplications =
  runFunctionForAllApps "recomputeQuestionnaireIndications" recomputeQuestionnaireIndications

recomputeQuestionnaireIndications :: AppContextM (ContextResult, Maybe String)
recomputeQuestionnaireIndications = do
  qtnUuids <- findQuestionnaireUuids
  traverse_ recomputeQuestionnaireIndication qtnUuids
  return (SuccessContextResult, Nothing)

recomputeQuestionnaireIndication :: U.UUID -> AppContextM ()
recomputeQuestionnaireIndication qtnUuid = do
  qtn <- findQuestionnaireById (U.toString qtnUuid)
  mPhasesAnsweredIndication <- getPhasesAnsweredIndication qtn
  case mPhasesAnsweredIndication of
    Just phasesAnsweredIndication -> updateQuestionnaireIndicationByUuid (U.toString qtnUuid) phasesAnsweredIndication
    Nothing ->
      logErrorU _CMP_SERVICE (f' "Can not get phasesAnsweredIndication for the qtn uuid: %s" [U.toString qtnUuid])
