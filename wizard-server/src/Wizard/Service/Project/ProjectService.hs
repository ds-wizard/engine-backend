module Wizard.Service.Project.ProjectService where

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
import Wizard.Api.Resource.Project.Detail.ProjectDetailDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Event.ProjectEventDTO
import Wizard.Api.Resource.Project.ProjectContentChangeDTO
import Wizard.Api.Resource.Project.ProjectCreateDTO
import Wizard.Api.Resource.Project.ProjectCreateFromTemplateDTO
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Api.Resource.Project.ProjectSettingsChangeDTO
import Wizard.Api.Resource.Project.ProjectShareChangeDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Project.ProjectCommentThreadDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Localization.Messages.Internal
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Project.Acl.ProjectAclHelpers
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.Detail.ProjectDetail
import Wizard.Model.Project.Detail.ProjectDetailPreview
import Wizard.Model.Project.Detail.ProjectDetailQuestionnaire
import Wizard.Model.Project.Detail.ProjectDetailSettings
import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Model.Project.File.ProjectFile
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectContent
import Wizard.Model.Project.ProjectReply
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService
import Wizard.Service.Mail.Mailer
import Wizard.Service.Project.Collaboration.ProjectCollaborationService
import Wizard.Service.Project.Comment.ProjectCommentService
import Wizard.Service.Project.Compiler.ProjectCompilerService
import Wizard.Service.Project.Event.ProjectEventMapper
import Wizard.Service.Project.File.ProjectFileService
import Wizard.Service.Project.ProjectAcl
import Wizard.Service.Project.ProjectAudit
import Wizard.Service.Project.ProjectMapper
import Wizard.Service.Project.ProjectUtil
import Wizard.Service.Project.ProjectValidation
import Wizard.Service.Project.Version.ProjectVersionService
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService
import WizardLib.Public.Model.PersistentCommand.Project.CreateProjectCommand

getProjectsForCurrentUserPageDto
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
  -> AppContextM (Page ProjectDTO)
getProjectsForCurrentUserPageDto mQuery mIsTemplate mIsMigrating mProjectTags mProjectTagsOp mUserUuids mUserUuidsOp mKnowledgeModelPackageIds mKnowledgeModelPackageIdsOp pageable sort = do
  checkPermission _PRJ_PERM
  currentUser <- getCurrentUser
  projectPage <-
    findProjectsForCurrentUserPage
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
  return . fmap toDTO' $ projectPage

createProject :: ProjectCreateDTO -> AppContextM ProjectDTO
createProject reqDto =
  liftIO generateUuid >>= createProjectWithGivenUuid reqDto

createProjectWithGivenUuid :: ProjectCreateDTO -> U.UUID -> AppContextM ProjectDTO
createProjectWithGivenUuid reqDto projectUuid =
  runInTransaction $ do
    checkProjectLimit
    checkCreatePermissionToProject
    pkgId <- resolvePackageId reqDto.knowledgeModelPackageId
    pkg <- findPackageById pkgId
    projectState <- getProjectState projectUuid pkgId
    now <- liftIO getCurrentTime
    tenantUuid <- asks currentTenantUuid
    visibility <- extractVisibility reqDto
    sharing <- extractSharing reqDto
    mCurrentUser <- asks currentUser
    knowledgeModel <- compileKnowledgeModel [] (Just pkgId) reqDto.questionTagUuids
    phaseEventUuid <- liftIO generateUuid
    let (project, projectEvents) =
          fromProjectCreateDTO
            reqDto
            projectUuid
            visibility
            sharing
            (fmap (.uuid) mCurrentUser)
            pkgId
            phaseEventUuid
            (headSafe knowledgeModel.phaseUuids)
            tenantUuid
            now
    insertProject project
    insertProjectEvents projectEvents
    permissionDtos <- traverse enhanceProjectPerm project.permissions
    return $ toSimpleDTO project pkg projectState permissionDtos

createProjectFromTemplate :: ProjectCreateFromTemplateDTO -> AppContextM ProjectDTO
createProjectFromTemplate reqDto =
  runInTransaction $ do
    checkProjectLimit
    originProject <- findProjectByUuid reqDto.projectUuid
    checkCreateFromTemplatePermissionToProject originProject.isTemplate
    pkg <- findPackageById originProject.knowledgeModelPackageId
    newProjectUuid <- liftIO generateUuid
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    tcProject <- getCurrentTenantConfigProject
    originProjectEvents <- findProjectEventListsByProjectUuid reqDto.projectUuid
    let newVisibility = tcProject.projectVisibility.defaultValue
    let newSharing = tcProject.projectSharing.defaultValue
    let newPermissions = [toUserProjectPerm newProjectUuid currentUser.uuid ownerPermissions tcProject.tenantUuid]
    let newProject =
          originProject
            { uuid = newProjectUuid
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
          :: Project
    insertProject newProject
    clonedFiles <- cloneProjectFiles originProject.uuid newProject.uuid
    newProjectEventsWithOldEventUuid <- cloneProjectEventsWithOldEventUuid originProjectEvents
    let newProjectEvents = fmap snd newProjectEventsWithOldEventUuid
    let newProjectEventsWithReplacedFiles = replaceProjectEventsWithNewFiles clonedFiles newProjectEvents
    insertProjectEvents (fmap (toEvent newProjectUuid newProject.tenantUuid) newProjectEventsWithReplacedFiles)
    duplicateCommentThreads reqDto.projectUuid newProjectUuid
    cloneProjectVersions originProject.uuid newProject.uuid newProjectEventsWithOldEventUuid
    state <- getProjectState newProjectUuid pkg.pId
    permissionDtos <- traverse enhanceProjectPerm newProject.permissions
    return $ toSimpleDTO newProject pkg state permissionDtos

cloneProject :: U.UUID -> AppContextM ProjectDTO
cloneProject cloneUuid =
  runInTransaction $ do
    checkProjectLimit
    originProject <- findProjectByUuid cloneUuid
    checkClonePermissionToProject originProject.visibility originProject.sharing originProject.permissions
    pkg <- findPackageById originProject.knowledgeModelPackageId
    newProjectUuid <- liftIO generateUuid
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    originProjectEvents <- findProjectEventListsByProjectUuid originProject.uuid
    let ownerPerm = toUserProjectPerm newProjectUuid currentUser.uuid ownerPermissions originProject.tenantUuid
    let newPermissions = ownerPerm : removeUserPermission currentUser.uuid originProject.permissions
    let newDuplicatedPermissions = fmap (\permission -> permission {projectUuid = newProjectUuid} :: ProjectPerm) newPermissions
    let newProject =
          originProject
            { uuid = newProjectUuid
            , name = "Copy of " ++ originProject.name
            , permissions = newDuplicatedPermissions
            , updatedAt = now
            }
          :: Project
    insertProject newProject
    clonedFiles <- cloneProjectFiles originProject.uuid newProject.uuid
    newProjectEventsWithOldEventUuid <- cloneProjectEventsWithOldEventUuid originProjectEvents
    let newProjectEvents = fmap snd newProjectEventsWithOldEventUuid
    let newProjectEventsWithReplacedFiles = replaceProjectEventsWithNewFiles clonedFiles newProjectEvents
    insertProjectEvents (fmap (toEvent newProjectUuid newProject.tenantUuid) newProjectEventsWithReplacedFiles)
    cloneProjectVersions originProject.uuid newProject.uuid newProjectEventsWithOldEventUuid
    duplicateCommentThreads cloneUuid newProjectUuid
    state <- getProjectState newProjectUuid pkg.pId
    permissionDtos <- traverse enhanceProjectPerm newProject.permissions
    return $ toSimpleDTO newProject pkg state permissionDtos

createProjectsFromCommands :: [CreateProjectCommand] -> AppContextM ()
createProjectsFromCommands = runInTransaction . traverse_ create
  where
    create :: CreateProjectCommand -> AppContextM ()
    create command = do
      uuid <- liftIO generateUuid
      currentUser <- getCurrentUser
      now <- liftIO getCurrentTime
      tcProject <- getCurrentTenantConfigProject
      users <- findUsersByEmails command.emails
      let permissions = fmap (createPermission uuid) users
      let project = fromCreateProjectCommand command uuid permissions tcProject currentUser.uuid now
      insertProject project
      return ()
    createPermission :: U.UUID -> User -> ProjectPerm
    createPermission projectUuid user = toUserProjectPerm projectUuid user.uuid ownerPermissions user.tenantUuid

getProjectById :: U.UUID -> AppContextM ProjectDTO
getProjectById projectUuid = do
  mProject <- getProjectById' projectUuid
  case mProject of
    Just project -> return project
    Nothing -> throwError $ NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "project" [("uuid", U.toString projectUuid)]

getProjectById' :: U.UUID -> AppContextM (Maybe ProjectDTO)
getProjectById' projectUuid = do
  mProject <- findProjectByUuid' projectUuid
  case mProject of
    Just project -> do
      checkViewPermissionToProject project.visibility project.sharing project.permissions
      package <- getPackageById project.knowledgeModelPackageId
      state <- getProjectState projectUuid package.pId
      permissionDtos <- traverse enhanceProjectPerm project.permissions
      return . Just $ toDTO project package state permissionDtos
    Nothing -> return Nothing

getProjectDetailByUuid :: U.UUID -> AppContextM ProjectDetailDTO
getProjectDetailByUuid projectUuid = do
  project <- findProjectDetail projectUuid
  checkViewPermissionToProject project.visibility project.sharing project.permissions
  return $ toDetailDTO project

getProjectDetailQuestionnaireByUuid :: U.UUID -> AppContextM ProjectDetailQuestionnaireDTO
getProjectDetailQuestionnaireByUuid projectUuid = do
  project <- findProjectDetailQuestionnaire projectUuid
  projectEvents <- findProjectEventListsByProjectUuid projectUuid
  checkViewPermissionToProject project.visibility project.sharing project.permissions
  editor <- catchError (hasEditPermissionToProject project.visibility project.sharing project.permissions) (\_ -> return False)
  commenter <- catchError (hasCommentPermissionToProject project.visibility project.sharing project.permissions) (\_ -> return False)
  unresolvedCommentCounts <-
    if commenter
      then findProjectCommentThreadsSimple projectUuid False editor
      else return M.empty
  resolvedCommentCounts <-
    if commenter
      then findProjectCommentThreadsSimple projectUuid True editor
      else return M.empty
  knowledgeModel <- compileKnowledgeModel [] (Just project.knowledgeModelPackageId) project.selectedQuestionTagUuids
  let projectContent = compileProjectEvents projectEvents
  let labels =
        if editor
          then projectContent.labels
          else M.empty
  return $ toDetailProjectDTO project unresolvedCommentCounts resolvedCommentCounts knowledgeModel projectContent.phaseUuid projectContent.replies labels

getProjectDetailPreviewById :: U.UUID -> AppContextM ProjectDetailPreview
getProjectDetailPreviewById projectUuid = do
  project <- findProjectDetailPreview projectUuid
  checkViewPermissionToProject project.visibility project.sharing project.permissions
  return project

getProjectDetailSettingsById :: U.UUID -> AppContextM ProjectDetailSettings
getProjectDetailSettingsById projectUuid = do
  project <- findProjectDetailSettings projectUuid
  checkViewPermissionToProject project.visibility project.sharing project.permissions
  knowledgeModel <- compileKnowledgeModel [] (Just project.knowledgeModelPackage.pId) project.selectedQuestionTagUuids
  return $ project {knowledgeModelTags = M.elems knowledgeModel.entities.tags}

getProjectEventsPage :: U.UUID -> Pageable -> [Sort] -> AppContextM (Page ProjectEventList)
getProjectEventsPage projectUuid pageable sort = do
  project <- findProjectByUuid projectUuid
  events <- findProjectEventsPage projectUuid pageable sort
  checkViewPermissionToProject project.visibility project.sharing project.permissions
  auditProjectListEvents projectUuid
  return events

getProjectEventForProjectUuid :: U.UUID -> U.UUID -> AppContextM ProjectEventDTO
getProjectEventForProjectUuid projectUuid eventUuid = do
  project <- findProjectByUuid projectUuid
  checkViewPermissionToProject project.visibility project.sharing project.permissions
  auditProjectDetailEvent projectUuid
  event <- findProjectEventByUuid eventUuid
  mUser <-
    case getCreatedBy event of
      Just userUuid -> findUserByUuid' userUuid
      Nothing -> return Nothing
  return $ toEventDTO event mUser

modifyProjectShare :: U.UUID -> ProjectShareChangeDTO -> AppContextM ProjectShareChangeDTO
modifyProjectShare projectUuid reqDto =
  runInTransaction $ do
    checkPermission _PRJ_PERM
    project <- findProjectByUuid projectUuid
    skipIfAssigningProject project (checkOwnerPermissionToProject project.visibility project.permissions)
    now <- liftIO getCurrentTime
    qVisibility <- extractVisibility reqDto
    qSharing <- extractSharing reqDto
    let updatedProject = fromShareChangeDTO project reqDto qVisibility qSharing now
    updateProjectByUuid updatedProject
    updatePermsForOnlineUsers projectUuid updatedProject.visibility updatedProject.sharing updatedProject.permissions
    permissionDtos <- traverse enhanceProjectPerm updatedProject.permissions
    skipIfAssigningProject
      project
      ( catchError
          (sendProjectInvitationMail project updatedProject)
          (\errMessage -> throwError $ GeneralServerError _ERROR_SERVICE_PROJECT__INVITATION_EMAIL_NOT_SENT)
      )
    mTemplate <-
      case updatedProject.documentTemplateId of
        Just tId -> do
          tml <- findDocumentTemplateById tId
          formats <- findDocumentTemplateFormats tId
          return . Just $ STM.toDTO tml formats
        _ -> return Nothing
    mFormat <-
      case (updatedProject.documentTemplateId, updatedProject.formatUuid) of
        (Just dtId, Just formatUuid) -> do
          format <- findDocumentTemplateFormatByDocumentTemplateIdAndUuid dtId formatUuid
          return $ Just format
        _ -> return Nothing
    projectEvents <- findProjectEventListsByProjectUuid projectUuid
    let projectContent = compileProjectEvents projectEvents
    unresolvedCommentCounts <- findProjectCommentThreadsSimple projectUuid False True
    resolvedCommentCounts <- findProjectCommentThreadsSimple projectUuid True True
    let restWsDto = toDetailWsDTO updatedProject mTemplate mFormat permissionDtos projectContent.labels unresolvedCommentCounts resolvedCommentCounts
    setProject projectUuid restWsDto
    return reqDto

modifyProjectSettings :: U.UUID -> ProjectSettingsChangeDTO -> AppContextM ProjectSettingsChangeDTO
modifyProjectSettings projectUuid reqDto =
  runInTransaction $ do
    checkPermission _PRJ_PERM
    validateProjectSettingsChangeDTO reqDto
    project <- findProjectByUuid projectUuid
    skipIfAssigningProject project (checkOwnerPermissionToProject project.visibility project.permissions)
    currentUser <- getCurrentUser
    now <- liftIO getCurrentTime
    let updatedProject = fromSettingsChangeDTO project reqDto currentUser now
    updateProjectByUuid updatedProject
    permissionDtos <- traverse enhanceProjectPerm updatedProject.permissions
    deleteTemporalDocumentsByProjectUuid project.uuid
    mTemplate <-
      case updatedProject.documentTemplateId of
        Just tId -> do
          tml <- findDocumentTemplateById tId
          formats <- findDocumentTemplateFormats tId
          return . Just $ STM.toDTO tml formats
        _ -> return Nothing
    mFormat <-
      case (updatedProject.documentTemplateId, updatedProject.formatUuid) of
        (Just dtId, Just formatUuid) -> do
          format <- findDocumentTemplateFormatByDocumentTemplateIdAndUuid dtId formatUuid
          return $ Just format
        _ -> return Nothing
    projectEvents <- findProjectEventListsByProjectUuid projectUuid
    let projectContent = compileProjectEvents projectEvents
    unresolvedCommentCounts <- findProjectCommentThreadsSimple projectUuid False True
    resolvedCommentCounts <- findProjectCommentThreadsSimple projectUuid True True
    let restWsDto = toDetailWsDTO updatedProject mTemplate mFormat permissionDtos projectContent.labels unresolvedCommentCounts resolvedCommentCounts
    setProject projectUuid restWsDto
    return reqDto

deleteProject :: U.UUID -> Bool -> AppContextM ()
deleteProject projectUuid shouldValidatePermission =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    validateProjectDeletion projectUuid
    when shouldValidatePermission (checkOwnerPermissionToProject project.visibility project.permissions)
    deleteProjectByUuid projectUuid
    void $ logOutOnlineUsersWhenProjectDramaticallyChanged projectUuid

modifyContent :: U.UUID -> ProjectContentChangeDTO -> AppContextM ProjectContentChangeDTO
modifyContent projectUuid reqDto =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkEditPermissionToProject project.visibility project.sharing project.permissions
    mCurrentUser <- asks currentUser
    now <- liftIO getCurrentTime
    projectEvents <- findProjectEventsByProjectUuid projectUuid
    let (updatedProject, updatedProjectEvents) = fromContentChangeDTO project projectEvents reqDto mCurrentUser now
    syncProjectEventsWithDb projectEvents updatedProjectEvents
    updateProjectSquashedAndUpdatedAtByUuid projectUuid False now
    return reqDto

cleanProjects :: AppContextM ()
cleanProjects =
  runInTransaction $ do
    projects <- findProjectsWithZeroAcl
    traverse_
      ( \project -> do
          logInfoI _CMP_SERVICE (f' "Clean project with empty ACL (projectUuid: '%s')" [U.toString project.uuid])
          deleteProject project.uuid False
      )
      projects

cloneProjectEvents :: [ProjectEventList] -> AppContextM [ProjectEventList]
cloneProjectEvents oldEvents = do
  newEvents <- cloneProjectEventsWithOldEventUuid oldEvents
  return $ fmap snd newEvents

cloneProjectEventsWithOldEventUuid :: [ProjectEventList] -> AppContextM [(U.UUID, ProjectEventList)]
cloneProjectEventsWithOldEventUuid =
  traverse
    ( \event -> do
        newEventUuid <- liftIO generateUuid
        return (getUuid event, setUuid event newEventUuid)
    )

replaceProjectEventsWithNewFiles :: [(ProjectFile, ProjectFile)] -> [ProjectEventList] -> [ProjectEventList]
replaceProjectEventsWithNewFiles clonedFiles projectEvents =
  let findFile :: U.UUID -> Maybe (ProjectFile, ProjectFile)
      findFile fileUuid = L.find (\(oldFile, newFile) -> oldFile.uuid == fileUuid) clonedFiles
      replaceEvent :: ProjectEventList -> ProjectEventList
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
   in fmap replaceEvent projectEvents
