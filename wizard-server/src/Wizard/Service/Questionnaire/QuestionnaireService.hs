module Wizard.Service.Questionnaire.QuestionnaireService where

import Control.Monad (void, when)
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
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import qualified Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper as STM
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleDTO
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
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Localization.Messages.Internal
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAclHelpers
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireDetail
import Wizard.Model.Questionnaire.QuestionnaireDetailPreview
import Wizard.Model.Questionnaire.QuestionnaireDetailQuestionnaire
import Wizard.Model.Questionnaire.QuestionnaireDetailSettings
import Wizard.Model.Questionnaire.QuestionnaireEventList
import Wizard.Model.Questionnaire.QuestionnaireFile
import Wizard.Model.Questionnaire.QuestionnairePerm
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Wizard.Service.Mail.Mailer
import Wizard.Service.Questionnaire.Collaboration.CollaborationService
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentService
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Service.Questionnaire.File.QuestionnaireFileService
import Wizard.Service.Questionnaire.QuestionnaireAcl
import Wizard.Service.Questionnaire.QuestionnaireAudit
import Wizard.Service.Questionnaire.QuestionnaireMapper
import Wizard.Service.Questionnaire.QuestionnaireUtil
import Wizard.Service.Questionnaire.QuestionnaireValidation
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionService
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService
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
getQuestionnairesForCurrentUserPageDto mQuery mIsTemplate mIsMigrating mProjectTags mProjectTagsOp mUserUuids mUserUuidsOp mKnowledgeModelPackageIds mKnowledgeModelPackageIdsOp pageable sort = do
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
      mKnowledgeModelPackageIds
      mKnowledgeModelPackageIdsOp
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
    pkgId <- resolvePackageId reqDto.knowledgeModelPackageId
    pkg <- findPackageById pkgId
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
    return $ toSimpleDTO qtn pkg qtnState permissionDtos

createQuestionnaireFromTemplate :: QuestionnaireCreateFromTemplateDTO -> AppContextM QuestionnaireDTO
createQuestionnaireFromTemplate reqDto =
  runInTransaction $ do
    checkQuestionnaireLimit
    originQtn <- findQuestionnaireByUuid reqDto.questionnaireUuid
    checkCreateFromTemplatePermissionToQtn originQtn.isTemplate
    pkg <- findPackageById originQtn.knowledgeModelPackageId
    newQtnUuid <- liftIO generateUuid
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    tcQuestionnaire <- getCurrentTenantConfigQuestionnaire
    originQtnEvents <- findQuestionnaireEventListsByQuestionnaireUuid reqDto.questionnaireUuid
    let newVisibility = tcQuestionnaire.questionnaireVisibility.defaultValue
    let newSharing = tcQuestionnaire.questionnaireSharing.defaultValue
    let newPermissions = [toUserQuestionnairePerm newQtnUuid currentUser.uuid ownerPermissions tcQuestionnaire.tenantUuid]
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
    newQtnEventsWithOldEventUuid <- cloneQuestionnaireEventsWithOldEventUuid originQtnEvents
    let newQtnEvents = fmap snd newQtnEventsWithOldEventUuid
    let newQtnEventsWithReplacedFiles = replaceQuestionnaireEventsWithNewFiles clonedFiles newQtnEvents
    insertQuestionnaireEvents (fmap (toEvent newQtnUuid newQtn.tenantUuid) newQtnEventsWithReplacedFiles)
    duplicateCommentThreads reqDto.questionnaireUuid newQtnUuid
    cloneQuestionnaireVersions originQtn.uuid newQtn.uuid newQtnEventsWithOldEventUuid
    state <- getQuestionnaireState newQtnUuid pkg.pId
    permissionDtos <- traverse enhanceQuestionnairePerm newQtn.permissions
    return $ toSimpleDTO newQtn pkg state permissionDtos

cloneQuestionnaire :: U.UUID -> AppContextM QuestionnaireDTO
cloneQuestionnaire cloneUuid =
  runInTransaction $ do
    checkQuestionnaireLimit
    originQtn <- findQuestionnaireByUuid cloneUuid
    checkClonePermissionToQtn originQtn.visibility originQtn.sharing originQtn.permissions
    pkg <- findPackageById originQtn.knowledgeModelPackageId
    newQtnUuid <- liftIO generateUuid
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    originQtnEvents <- findQuestionnaireEventListsByQuestionnaireUuid originQtn.uuid
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
    newQtnEventsWithOldEventUuid <- cloneQuestionnaireEventsWithOldEventUuid originQtnEvents
    let newQtnEvents = fmap snd newQtnEventsWithOldEventUuid
    let newQtnEventsWithReplacedFiles = replaceQuestionnaireEventsWithNewFiles clonedFiles newQtnEvents
    insertQuestionnaireEvents (fmap (toEvent newQtnUuid newQtn.tenantUuid) newQtnEventsWithReplacedFiles)
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
      tcQuestionnaire <- getCurrentTenantConfigQuestionnaire
      users <- findUsersByEmails command.emails
      let permissions = fmap (createPermission uuid) users
      let questionnaire = fromCreateQuestionnaireCommand command uuid permissions tcQuestionnaire currentUser.uuid now
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
      package <- getPackageById qtn.knowledgeModelPackageId
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
  qtnEvents <- findQuestionnaireEventListsByQuestionnaireUuid qtnUuid
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
  knowledgeModel <- compileKnowledgeModel [] (Just qtn.knowledgeModelPackageId) qtn.selectedQuestionTagUuids
  let qtnCtn = compileQuestionnaire qtnEvents
  let labels =
        if editor
          then qtnCtn.labels
          else M.empty
  return $ toDetailQuestionnaireDTO qtn unresolvedCommentCounts resolvedCommentCounts knowledgeModel qtnCtn.phaseUuid qtnCtn.replies labels

getQuestionnaireDetailPreviewById :: U.UUID -> AppContextM QuestionnaireDetailPreview
getQuestionnaireDetailPreviewById qtnUuid = do
  qtn <- findQuestionnaireDetailPreview qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  return qtn

getQuestionnaireDetailSettingsById :: U.UUID -> AppContextM QuestionnaireDetailSettings
getQuestionnaireDetailSettingsById qtnUuid = do
  qtn <- findQuestionnaireDetailSettings qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  knowledgeModel <- compileKnowledgeModel [] (Just qtn.knowledgeModelPackage.pId) qtn.selectedQuestionTagUuids
  return $ qtn {knowledgeModelTags = M.elems knowledgeModel.entities.tags}

getQuestionnaireEventsPage :: U.UUID -> Pageable -> [Sort] -> AppContextM (Page QuestionnaireEventList)
getQuestionnaireEventsPage qtnUuid pageable sort = do
  qtn <- findQuestionnaireByUuid qtnUuid
  events <- findQuestionnaireEventsPage qtnUuid pageable sort
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  auditQuestionnaireListEvents qtnUuid
  return events

getQuestionnaireEventForQtnUuid :: U.UUID -> U.UUID -> AppContextM QuestionnaireEventDTO
getQuestionnaireEventForQtnUuid qtnUuid eventUuid = do
  qtn <- findQuestionnaireByUuid qtnUuid
  checkViewPermissionToQtn qtn.visibility qtn.sharing qtn.permissions
  auditQuestionnaireDetailEvent qtnUuid
  event <- findQuestionnaireEventByUuid eventUuid
  mUser <-
    case getCreatedBy event of
      Just userUuid -> findUserByUuid' userUuid
      Nothing -> return Nothing
  return $ toEventDTO event mUser

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
          formats <- findDocumentTemplateFormats tId
          return . Just $ STM.toDTO tml formats
        _ -> return Nothing
    mFormat <-
      case (updatedQtn.documentTemplateId, updatedQtn.formatUuid) of
        (Just dtId, Just formatUuid) -> do
          format <- findDocumentTemplateFormatByDocumentTemplateIdAndUuid dtId formatUuid
          return $ Just format
        _ -> return Nothing
    qtnEvents <- findQuestionnaireEventListsByQuestionnaireUuid qtnUuid
    let qtnCtn = compileQuestionnaire qtnEvents
    unresolvedCommentCounts <- findQuestionnaireCommentThreadsSimple qtnUuid False True
    resolvedCommentCounts <- findQuestionnaireCommentThreadsSimple qtnUuid True True
    let restWsDto = toDetailWsDTO updatedQtn mTemplate mFormat permissionDtos qtnCtn.labels unresolvedCommentCounts resolvedCommentCounts
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
          formats <- findDocumentTemplateFormats tId
          return . Just $ STM.toDTO tml formats
        _ -> return Nothing
    mFormat <-
      case (updatedQtn.documentTemplateId, updatedQtn.formatUuid) of
        (Just dtId, Just formatUuid) -> do
          format <- findDocumentTemplateFormatByDocumentTemplateIdAndUuid dtId formatUuid
          return $ Just format
        _ -> return Nothing
    qtnEvents <- findQuestionnaireEventListsByQuestionnaireUuid qtnUuid
    let qtnCtn = compileQuestionnaire qtnEvents
    unresolvedCommentCounts <- findQuestionnaireCommentThreadsSimple qtnUuid False True
    resolvedCommentCounts <- findQuestionnaireCommentThreadsSimple qtnUuid True True
    let restWsDto = toDetailWsDTO updatedQtn mTemplate mFormat permissionDtos qtnCtn.labels unresolvedCommentCounts resolvedCommentCounts
    setQuestionnaire qtnUuid restWsDto
    return reqDto

deleteQuestionnaire :: U.UUID -> Bool -> AppContextM ()
deleteQuestionnaire qtnUuid shouldValidatePermission =
  runInTransaction $ do
    qtn <- findQuestionnaireByUuid qtnUuid
    validateQuestionnaireDeletion qtnUuid
    when shouldValidatePermission (checkOwnerPermissionToQtn qtn.visibility qtn.permissions)
    deleteQuestionnaireByUuid qtnUuid
    void $ logOutOnlineUsersWhenQtnDramaticallyChanged qtnUuid

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

cloneQuestionnaireEvents :: [QuestionnaireEventList] -> AppContextM [QuestionnaireEventList]
cloneQuestionnaireEvents oldEvents = do
  newEvents <- cloneQuestionnaireEventsWithOldEventUuid oldEvents
  return $ fmap snd newEvents

cloneQuestionnaireEventsWithOldEventUuid :: [QuestionnaireEventList] -> AppContextM [(U.UUID, QuestionnaireEventList)]
cloneQuestionnaireEventsWithOldEventUuid =
  traverse
    ( \event -> do
        newEventUuid <- liftIO generateUuid
        return (getUuid event, setUuid event newEventUuid)
    )

replaceQuestionnaireEventsWithNewFiles :: [(QuestionnaireFile, QuestionnaireFile)] -> [QuestionnaireEventList] -> [QuestionnaireEventList]
replaceQuestionnaireEventsWithNewFiles clonedFiles qtnEvents =
  let findFile :: U.UUID -> Maybe (QuestionnaireFile, QuestionnaireFile)
      findFile fileUuid = L.find (\(oldFile, newFile) -> oldFile.uuid == fileUuid) clonedFiles
      replaceEvent :: QuestionnaireEventList -> QuestionnaireEventList
      replaceEvent (SetReplyEventList' event) =
        let value' =
              case event.value of
                r@FileReply {..} ->
                  case findFile r.fValue of
                    Just (oldFile, newFile) -> r {fValue = newFile.uuid}
                    _ -> r
                r -> r
         in SetReplyEventList' (event {value = value'})
      replaceEvent event' = event'
   in fmap replaceEvent qtnEvents
