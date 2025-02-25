module Wizard.Service.Questionnaire.QuestionnaireService where

import Control.Monad (when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Common.Lens
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Error.Error
import Shared.Common.Util.List
import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSettingsChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireShareChangeDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
import Wizard.Database.DAO.Questionnaire.QuestionnairePermDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireVersionDAO
import Wizard.Database.DAO.Submission.SubmissionDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Localization.Messages.Internal
import Wizard.Model.Common.Lens
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAclHelpers
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireDetail
import Wizard.Model.Questionnaire.QuestionnaireDetailPreview
import Wizard.Model.Questionnaire.QuestionnaireDetailQuestionnaire
import Wizard.Model.Questionnaire.QuestionnaireDetailSettings
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireFile
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.S3.Document.DocumentS3
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Mail.Mailer
import Wizard.Service.Package.PackageService
import Wizard.Service.Questionnaire.Collaboration.CollaborationService
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.File.QuestionnaireFileService
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireAudit
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireUtil
import Wizard.Service.Questionnaire.QuestionnaireValidation
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionService
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import WizardLib.KnowledgeModel.Service.Package.PackageUtil
import WizardLib.Public.Model.PersistentCommand.Questionnaire.CreateQuestionnaireCommand

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
    qtnState <- getQuestionnaireState qtnUuid pkgId
    now <- liftIO getCurrentTime
    tenantUuid <- asks currentTenantUuid
    visibility <- extractVisibility reqDto
    sharing <- extractSharing reqDto
    mCurrentUser <- asks currentUser
    knowledgeModel <- compileKnowledgeModel [] (Just pkgId) reqDto.questionTagUuids
    phaseEventUuid <- liftIO generateUuid
    let (qtn, qtnEvents) =
          fromQuestionnaireCreateDTO
            reqDto
            qtnUuid
            visibility
            sharing
            (fmap (.uuid) mCurrentUser)
            pkgId
            phaseEventUuid
            (headSafe knowledgeModel.phaseUuids)
            tenantUuid
            now
    insertQuestionnaire qtn
    insertQuestionnaireEvents qtnEvents
    permissionDtos <- traverse enhanceQuestionnairePerm qtn.permissions
    qtnCtn <- compileQuestionnaire qtnEvents
    return $ toSimpleDTO qtn package qtnState permissionDtos

createQuestionnaireFromTemplate :: QuestionnaireCreateFromTemplateDTO -> AppContextM QuestionnaireDTO
createQuestionnaireFromTemplate reqDto =
  runInTransaction $ do
    checkQuestionnaireLimit
    originQtn <- findQuestionnaireByUuid reqDto.questionnaireUuid
    checkCreateFromTemplatePermissionToQtn originQtn.isTemplate
    pkg <- findPackageWithEventsById originQtn.packageId
    newQtnUuid <- liftIO generateUuid
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    tenantConfig <- getCurrentTenantConfig
    originQtnEvents <- findQuestionnaireEventsByQuestionnaireUuid reqDto.questionnaireUuid
    let newVisibility = tenantConfig.questionnaire.questionnaireVisibility.defaultValue
    let newSharing = tenantConfig.questionnaire.questionnaireSharing.defaultValue
    let newPermissions = [toUserQuestionnairePerm newQtnUuid currentUser.uuid ownerPermissions tenantConfig.uuid]
    let newQtn =
          originQtn
            { uuid = newQtnUuid
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
    clonedFiles <- cloneQuestionnaireFiles originQtn.uuid newQtn.uuid
    newQtnEventsWithOldEventUuid <- cloneQuestinonaireEventsWithOldEventUuid newQtnUuid originQtnEvents
    let newQtnEvents = fmap snd newQtnEventsWithOldEventUuid
    let newQtnEventsWithReplacedFiles = replaceQuestinonaireEventsWithNewFiles clonedFiles newQtnEvents
    insertQuestionnaireEvents newQtnEventsWithReplacedFiles
    duplicateCommentThreads reqDto.questionnaireUuid newQtnUuid
    cloneQuestionnaireVersions originQtn.uuid newQtn.uuid newQtnEventsWithOldEventUuid
    state <- getQuestionnaireState newQtnUuid pkg.pId
    permissionDtos <- traverse enhanceQuestionnairePerm newQtn.permissions
    qtnCtn <- compileQuestionnaire newQtnEventsWithReplacedFiles
    return $ toSimpleDTO newQtn pkg state permissionDtos

cloneQuestionnaire :: U.UUID -> AppContextM QuestionnaireDTO
cloneQuestionnaire cloneUuid =
  runInTransaction $ do
    checkQuestionnaireLimit
    originQtn <- findQuestionnaireByUuid cloneUuid
    checkClonePermissionToQtn originQtn.visibility originQtn.sharing originQtn.permissions
    pkg <- findPackageWithEventsById originQtn.packageId
    newQtnUuid <- liftIO generateUuid
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    originQtnEvents <- findQuestionnaireEventsByQuestionnaireUuid originQtn.uuid
    let ownerPerm = toUserQuestionnairePerm newQtnUuid currentUser.uuid ownerPermissions originQtn.tenantUuid
    let newPermissions = ownerPerm : removeUserPermission currentUser.uuid originQtn.permissions
    let newDuplicatedPermissions = fmap (\permission -> permission {questionnaireUuid = newQtnUuid} :: QuestionnairePerm) newPermissions
    let newQtn =
          originQtn
            { uuid = newQtnUuid
            , name = "Copy of " ++ originQtn.name
            , permissions = newDuplicatedPermissions
            , updatedAt = now
            }
          :: Questionnaire
    insertQuestionnaire newQtn
    clonedFiles <- cloneQuestionnaireFiles originQtn.uuid newQtn.uuid
    newQtnEventsWithOldEventUuid <- cloneQuestinonaireEventsWithOldEventUuid newQtnUuid originQtnEvents
    let newQtnEvents = fmap snd newQtnEventsWithOldEventUuid
    let newQtnEventsWithReplacedFiles = replaceQuestinonaireEventsWithNewFiles clonedFiles newQtnEvents
    insertQuestionnaireEvents newQtnEventsWithReplacedFiles
    cloneQuestionnaireVersions originQtn.uuid newQtn.uuid newQtnEventsWithOldEventUuid
    duplicateCommentThreads cloneUuid newQtnUuid
    state <- getQuestionnaireState newQtnUuid pkg.pId
    permissionDtos <- traverse enhanceQuestionnairePerm newQtn.permissions
    return $ toSimpleDTO newQtn pkg state permissionDtos

createQuestionnairesFromCommands :: [CreateQuestionnaireCommand] -> AppContextM ()
createQuestionnairesFromCommands = runInTransaction . traverse_ create
  where
    create :: CreateQuestionnaireCommand -> AppContextM ()
    create command = do
      uuid <- liftIO generateUuid
      currentUser <- getCurrentUser
      now <- liftIO getCurrentTime
      tenantConfig <- getCurrentTenantConfig
      users <- findUsersByEmails command.emails
      let permissions = fmap (createPermission uuid) users
      let questionnaire = fromCreateQuestionnaireCommand command uuid permissions tenantConfig currentUser.uuid now
      insertQuestionnaire questionnaire
      return ()
    createPermission :: U.UUID -> User -> QuestionnairePerm
    createPermission questionnaireUuid user = toUserQuestionnairePerm questionnaireUuid user.uuid ownerPermissions user.tenantUuid

getQuestionnaireById :: U.UUID -> AppContextM QuestionnaireDTO
getQuestionnaireById qtnUuid = do
  mQtn <- getQuestionnaireById' qtnUuid
  case mQtn of
    Just qtn -> return qtn
    Nothing -> throwError $ NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "questionnaire" [("uuid", U.toString qtnUuid)]

getQuestionnaireById' :: U.UUID -> AppContextM (Maybe QuestionnaireDTO)
getQuestionnaireById' qtnUuid = do
  mQtn <- findQuestionnaireByUuid' qtnUuid
  case mQtn of
    Just qtn -> do
      checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
      package <- getPackageById qtn.packageId
      state <- getQuestionnaireState qtnUuid package.pId
      permissionDtos <- traverse enhanceQuestionnairePerm qtn.permissions
      return . Just $ toDTO qtn package state permissionDtos
    Nothing -> return Nothing

getQuestionnaireDetailByUuid :: U.UUID -> AppContextM QuestionnaireDetailDTO
getQuestionnaireDetailByUuid qtnUuid = do
  qtn <- findQuestionnaireDetail qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  return $ toDetailDTO qtn

getQuestionnaireDetailQuestionnaireByUuid :: U.UUID -> AppContextM QuestionnaireDetailQuestionnaireDTO
getQuestionnaireDetailQuestionnaireByUuid qtnUuid = do
  qtn <- findQuestionnaireDetailQuestionnaire qtnUuid
  qtnEvents <- findQuestionnaireEventsByQuestionnaireUuid qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  editor <- catchError (hasEditPermissionToQtn qtn.visibility qtn.sharing qtn.permissions) (\_ -> return False)
  commenter <- catchError (hasCommentPermissionToQtn qtn.visibility qtn.sharing qtn.permissions) (\_ -> return False)
  unresolvedCommentCounts <-
    if commenter
      then findQuestionnaireCommentThreadsSimple qtnUuid False editor
      else return M.empty
  resolvedCommentCounts <-
    if commenter
      then findQuestionnaireCommentThreadsSimple qtnUuid True editor
      else return M.empty
  knowledgeModel <- compileKnowledgeModel [] (Just qtn.packageId) qtn.selectedQuestionTagUuids
  qtnCtn <- compileQuestionnaire qtnEvents
  return $ toDetailQuestionnaireDTO qtn unresolvedCommentCounts resolvedCommentCounts knowledgeModel qtnCtn

getQuestionnaireDetailPreviewById :: U.UUID -> AppContextM QuestionnaireDetailPreview
getQuestionnaireDetailPreviewById qtnUuid = do
  qtn <- findQuestionnaireDetailPreview qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  return qtn

getQuestionnaireDetailSettingsById :: U.UUID -> AppContextM QuestionnaireDetailSettings
getQuestionnaireDetailSettingsById qtnUuid = do
  qtn <- findQuestionnaireDetailSettings qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  knowledgeModel <- compileKnowledgeModel [] (Just qtn.package.pId) qtn.selectedQuestionTagUuids
  return $ qtn {knowledgeModelTags = M.elems knowledgeModel.entities.tags}

getQuestionnaireEventsForQtnUuid :: U.UUID -> AppContextM [QuestionnaireEventDTO]
getQuestionnaireEventsForQtnUuid qtnUuid = do
  qtn <- findQuestionnaireByUuid qtnUuid
  events <- findQuestionnaireEventsByQuestionnaireUuid qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  auditQuestionnaireListEvents qtnUuid
  traverse enhanceQuestionnaireEvent events

getQuestionnaireEventForQtnUuid :: U.UUID -> U.UUID -> AppContextM QuestionnaireEventDTO
getQuestionnaireEventForQtnUuid qtnUuid eventUuid = do
  qtn <- findQuestionnaireByUuid qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  auditQuestionnaireDetailEvent qtnUuid
  event <- findQuestionnaireEventByUuid eventUuid
  enhanceQuestionnaireEvent event

modifyQuestionnaireShare :: U.UUID -> QuestionnaireShareChangeDTO -> AppContextM QuestionnaireShareChangeDTO
modifyQuestionnaireShare qtnUuid reqDto =
  runInTransaction $ do
    checkPermission _QTN_PERM
    qtn <- findQuestionnaireByUuid qtnUuid
    skipIfAssigningProject qtn (checkOwnerPermissionToQtn qtn.visibility qtn.permissions)
    now <- liftIO getCurrentTime
    qVisibility <- extractVisibility reqDto
    qSharing <- extractSharing reqDto
    let updatedQtn = fromShareChangeDTO qtn reqDto qVisibility qSharing now
    updateQuestionnaireByUuid updatedQtn
    updatePermsForOnlineUsers qtnUuid updatedQtn.visibility updatedQtn.sharing updatedQtn.permissions
    permissionDtos <- traverse enhanceQuestionnairePerm updatedQtn.permissions
    skipIfAssigningProject
      qtn
      ( catchError
          (sendQuestionnaireInvitationMail qtn updatedQtn)
          (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_QTN__INVITATION_EMAIL_NOT_SENT)
      )
    mTemplate <-
      case updatedQtn.documentTemplateId of
        Just tId -> do
          tml <- findDocumentTemplateById tId
          return $ Just tml
        _ -> return Nothing
    mFormat <-
      case (mTemplate, updatedQtn.formatUuid) of
        (Just tml, Just fUuid) -> return $ L.find (\f -> f.uuid == fUuid) tml.formats
        _ -> return Nothing
    let restWsDto = toDetailWsDTO updatedQtn mTemplate mFormat permissionDtos
    setQuestionnaire qtnUuid restWsDto
    return reqDto

modifyQuestionnaireSettings :: U.UUID -> QuestionnaireSettingsChangeDTO -> AppContextM QuestionnaireSettingsChangeDTO
modifyQuestionnaireSettings qtnUuid reqDto =
  runInTransaction $ do
    checkPermission _QTN_PERM
    validateQuestionnaireSettingsChangeDTO reqDto
    qtn <- findQuestionnaireByUuid qtnUuid
    skipIfAssigningProject qtn (checkOwnerPermissionToQtn qtn.visibility qtn.permissions)
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    let updatedQtn = fromSettingsChangeDTO qtn reqDto currentUser now
    updateQuestionnaireByUuid updatedQtn
    permissionDtos <- traverse enhanceQuestionnairePerm updatedQtn.permissions
    deleteTemporalDocumentsByQuestionnaireUuid qtn.uuid
    mTemplate <-
      case updatedQtn.documentTemplateId of
        Just tId -> do
          tml <- findDocumentTemplateById tId
          return $ Just tml
        _ -> return Nothing
    mFormat <-
      case (mTemplate, updatedQtn.formatUuid) of
        (Just tml, Just fUuid) -> return $ L.find (\f -> f.uuid == fUuid) tml.formats
        _ -> return Nothing
    let restWsDto = toDetailWsDTO updatedQtn mTemplate mFormat permissionDtos
    setQuestionnaire qtnUuid restWsDto
    return reqDto

deleteQuestionnaire :: U.UUID -> Bool -> AppContextM ()
deleteQuestionnaire qtnUuid shouldValidatePermission =
  runInTransaction $ do
    unsetQuestionnaireFromDocumentTemplate qtnUuid
    qtn <- findQuestionnaireByUuid qtnUuid
    validateQuestionnaireDeletation qtnUuid
    when shouldValidatePermission (checkOwnerPermissionToQtn qtn.visibility qtn.permissions)
    deleteMigratorStateByNewQuestionnaireUuid qtnUuid
    threads <- findQuestionnaireCommentThreads qtnUuid
    traverse_
      ( \t -> do
          deleteQuestionnaireCommentsByThreadUuid t.uuid
          deleteQuestionnaireCommentThreadById t.uuid
      )
      threads
    documents <- findDocumentsForCurrentTenantFiltered [("questionnaire_uuid", U.toString qtnUuid)]
    traverse_
      ( \d -> do
          deleteSubmissionsFiltered [("document_uuid", U.toString d.uuid)]
          deleteDocumentsFiltered [("uuid", U.toString d.uuid)]
          removeDocumentContent d.uuid
      )
      documents
    deleteQuestionnaireVersionsByQuestionnaireUuid qtnUuid
    deleteQuestionnaireFilesByQuestionnaireUuid qtnUuid
    deleteQuestionnairePermsFiltered [("questionnaire_uuid", U.toString qtnUuid)]
    deleteQuestionnaireEventsByQuestionnaireUuid qtnUuid
    deleteQuestionnaireByUuid qtnUuid
    logOutOnlineUsersWhenQtnDramaticallyChanged qtnUuid
    return ()

modifyContent :: U.UUID -> QuestionnaireContentChangeDTO -> AppContextM QuestionnaireContentChangeDTO
modifyContent qtnUuid reqDto =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid qtnUuid
    checkEditPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
    mCurrentUser <- asks currentUser
    now <- liftIO getCurrentTime
    qtnEvents <- findQuestionnaireEventsByQuestionnaireUuid qtnUuid
    let (updatedQtn, updatedQtnEvents) = fromContentChangeDTO qtn qtnEvents reqDto mCurrentUser now
    syncQuestionnaireEventsWithDb qtnEvents updatedQtnEvents
    updateQuestionnaireSquashedAndUpdatedAtByUuid qtnUuid False now
    return reqDto

cleanQuestionnaires :: AppContextM ()
cleanQuestionnaires =
  runInTransaction $ do
    qtns <- findQuestionnaireWithZeroAcl
    traverse_
      ( \qtn -> do
          logInfoI _CMP_SERVICE (f' "Clean questionnaire with empty ACL (qtnUuid: '%s')" [U.toString qtn.uuid])
          deleteQuestionnaire qtn.uuid False
      )
      qtns

cloneQuestinonaireEvents :: U.UUID -> [QuestionnaireEvent] -> AppContextM [QuestionnaireEvent]
cloneQuestinonaireEvents newQtnUuid oldEvents = do
  newEvents <- cloneQuestinonaireEventsWithOldEventUuid newQtnUuid oldEvents
  return $ fmap snd newEvents

cloneQuestinonaireEventsWithOldEventUuid :: U.UUID -> [QuestionnaireEvent] -> AppContextM [(U.UUID, QuestionnaireEvent)]
cloneQuestinonaireEventsWithOldEventUuid newQtnUuid =
  traverse
    ( \event -> do
        newEventUuid <- liftIO generateUuid
        return (getUuid event, setQuestionnaireUuid (setUuid event newEventUuid) newQtnUuid)
    )

replaceQuestinonaireEventsWithNewFiles :: [(QuestionnaireFile, QuestionnaireFile)] -> [QuestionnaireEvent] -> [QuestionnaireEvent]
replaceQuestinonaireEventsWithNewFiles clonedFiles qtnEvents =
  let findFile :: U.UUID -> Maybe (QuestionnaireFile, QuestionnaireFile)
      findFile fileUuid = L.find (\(oldFile, newFile) -> oldFile.uuid == fileUuid) clonedFiles
      replaceEvent :: QuestionnaireEvent -> QuestionnaireEvent
      replaceEvent (SetReplyEvent' event) =
        let value' =
              case event.value of
                r@FileReply {..} ->
                  case findFile r.fValue of
                    Just (oldFile, newFile) -> r {fValue = newFile.uuid}
                    _ -> r
                r -> r
         in SetReplyEvent' (event {value = value'})
      replaceEvent event' = event'
   in fmap replaceEvent qtnEvents
