module Wizard.Service.Project.Migration.ProjectMigrationService where

import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.List
import Shared.Common.Util.Uuid
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationChangeDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Project.ProjectMigrationDAO
import Wizard.Database.DAO.Project.ProjectVersionDAO
import Wizard.Model.Common.Lens
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.Event.ProjectEvent
import Wizard.Model.Project.Migration.ProjectMigration
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectContent
import Wizard.Model.Project.Version.ProjectVersion
import Wizard.Service.DocumentTemplate.DocumentTemplateUtil
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Project.Compiler.ProjectCompilerService
import Wizard.Service.Project.Event.ProjectEventMapper
import Wizard.Service.Project.Migration.Migrator.Sanitizer
import Wizard.Service.Project.Migration.ProjectMigrationAudit
import Wizard.Service.Project.Migration.ProjectMigrationMapper
import Wizard.Service.Project.Migration.ProjectMigrationValidation
import Wizard.Service.Project.ProjectAcl
import Wizard.Service.Project.ProjectService

createProjectMigration :: U.UUID -> ProjectMigrationCreateDTO -> AppContextM ProjectMigrationDTO
createProjectMigration oldProjectUuid reqDto =
  runInTransaction $ do
    checkPermission _PRJ_PERM
    validateMigrationExistence oldProjectUuid
    oldProject <- findProjectByUuid oldProjectUuid
    checkMigrationPermissionToProject oldProject.visibility oldProject.permissions
    (newProject, newProjectEvents, newProjectVersions) <- upgradeProject reqDto oldProject
    insertProject newProject
    insertProjectEvents newProjectEvents
    traverse_ insertProjectVersion newProjectVersions
    tenantUuid <- asks currentTenantUuid
    let projectMigration = fromCreateDTO oldProject.uuid newProject.uuid tenantUuid
    insertProjectMigration projectMigration
    auditProjectMigrationCreate reqDto oldProject newProject
    getProjectMigration newProject.uuid

getProjectMigration :: U.UUID -> AppContextM ProjectMigrationDTO
getProjectMigration projectUuid = do
  checkPermission _PRJ_PERM
  projectMigration <- findProjectMigrationByNewProjectUuid projectUuid
  oldProjectDto <- getProjectDetailQuestionnaireByUuid projectMigration.oldProjectUuid
  newProjectDto <- getProjectDetailQuestionnaireByUuid projectMigration.newProjectUuid
  oldProject <- findProjectByUuid projectMigration.oldProjectUuid
  newProject <- findProjectByUuid projectMigration.newProjectUuid
  checkMigrationPermissionToProject oldProject.visibility oldProject.permissions
  checkMigrationPermissionToProject newProject.visibility newProject.permissions
  return $ toDTO oldProjectDto newProjectDto projectMigration.resolvedQuestionUuids projectMigration.tenantUuid

modifyProjectMigration :: U.UUID -> ProjectMigrationChangeDTO -> AppContextM ProjectMigrationDTO
modifyProjectMigration projectUuid reqDto =
  runInTransaction $ do
    checkPermission _PRJ_PERM
    projectMigration <- getProjectMigration projectUuid
    let updatedState = fromChangeDTO reqDto projectMigration
    updateProjectMigrationByNewProjectUuid updatedState
    auditProjectMigrationModify projectMigration reqDto
    return $ toDTO projectMigration.oldProject projectMigration.newProject updatedState.resolvedQuestionUuids updatedState.tenantUuid

finishProjectMigration :: U.UUID -> AppContextM ()
finishProjectMigration projectUuid =
  runInTransaction $ do
    checkPermission _PRJ_PERM
    _ <- getProjectMigration projectUuid
    projectMigration <- findProjectMigrationByNewProjectUuid projectUuid
    deleteProjectMigrationByNewProjectUuid projectUuid
    oldProject <- findProjectByUuid projectMigration.oldProjectUuid
    newProject <- findProjectByUuid projectMigration.newProjectUuid
    newProjectEvents <- ensurePhaseIsSetIfNecessary newProject
    newProjectVersions <- findProjectVersionsByProjectUuid projectMigration.newProjectUuid
    now <- liftIO getCurrentTime
    let newProjectUpdated =
          oldProject
            { formatUuid = newProject.formatUuid
            , documentTemplateId = newProject.documentTemplateId
            , selectedQuestionTagUuids = newProject.selectedQuestionTagUuids
            , knowledgeModelPackageId = newProject.knowledgeModelPackageId
            , updatedAt = now
            }
          :: Project
    let newProjectEventsWithOldProjectUuid = fmap (\event -> setProjectUuid event oldProject.uuid) newProjectEvents
    newVersionsWithNewUuid <- traverse generateNewVersionUuid newProjectVersions
    let newVersionsWithOldProjectUuid = fmap (\v -> v {projectUuid = oldProject.uuid} :: ProjectVersion) newVersionsWithNewUuid
    -- Delete the new project
    deleteProjectEventsByProjectUuid newProject.uuid
    deleteProject newProject.uuid False
    -- Update the old project with values from new project
    updateProjectByUuid newProjectUpdated
    deleteProjectEventsByProjectUuid oldProject.uuid
    insertProjectEvents newProjectEventsWithOldProjectUuid
    traverse_ insertProjectVersion newVersionsWithOldProjectUuid
    auditProjectMigrationFinish oldProject newProject

cancelProjectMigration :: U.UUID -> AppContextM ()
cancelProjectMigration projectUuid =
  runInTransaction $ do
    checkPermission _PRJ_PERM
    projectMigration <- getProjectMigration projectUuid
    deleteProject projectMigration.newProject.uuid True
    deleteProjectMigrationByNewProjectUuid projectUuid
    auditProjectMigrationCancel projectMigration
    return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
upgradeProject :: ProjectMigrationCreateDTO -> Project -> AppContextM (Project, [ProjectEvent], [ProjectVersion])
upgradeProject reqDto oldProject = do
  let newPkgId = reqDto.targetKnowledgeModelPackageId
  let newTagUuids = reqDto.targetTagUuids
  oldKm <- compileKnowledgeModel [] (Just oldProject.knowledgeModelPackageId) newTagUuids
  newKm <- compileKnowledgeModel [] (Just newPkgId) newTagUuids
  newUuid <- liftIO generateUuid
  oldProjectEvents <- findProjectEventListsByProjectUuid oldProject.uuid
  clonedProjectEventsWithOldEventUuid <- cloneProjectEventsWithOldEventUuid oldProjectEvents
  let clonedProjectEvents = fmap snd clonedProjectEventsWithOldEventUuid
  newProjectEvents <- sanitizeProjectEvents newUuid oldKm newKm clonedProjectEvents
  (newDocumentTemplateId, newFormatUuid) <- getNewDocumentTemplateIdAndFormatUuid oldProject newPkgId
  let newProjectEventUuids = fmap getUuid newProjectEvents
  let clonedProjectEventsFiltered = filter (\e -> getUuid (snd e) `elem` newProjectEventUuids) clonedProjectEventsWithOldEventUuid
  let newPermissions = fmap (\perm -> perm {projectUuid = newUuid} :: ProjectPerm) oldProject.permissions
  let upgradedProject =
        oldProject
          { uuid = newUuid
          , knowledgeModelPackageId = newPkgId
          , selectedQuestionTagUuids = newTagUuids
          , documentTemplateId = newDocumentTemplateId
          , formatUuid = newFormatUuid
          , permissions = newPermissions
          }
        :: Project
  versionsWithOldProjectUuid <- findProjectVersionsByProjectUuid oldProject.uuid
  newVersionsWithNewUuid <- traverse generateNewVersionUuid versionsWithOldProjectUuid
  let newVersionsWithNewEventUuid =
        fmap
          ( \v ->
              case L.find (\(oldEventUuid, _) -> v.eventUuid == oldEventUuid) clonedProjectEventsWithOldEventUuid of
                Just (_, newEvent) ->
                  Just $
                    v
                      { projectUuid = newUuid
                      , eventUuid = getUuid newEvent
                      }
                Nothing -> Nothing
          )
          newVersionsWithNewUuid
  let newVersions = catMaybes newVersionsWithNewEventUuid
  return (upgradedProject, fmap (toEvent upgradedProject.uuid upgradedProject.tenantUuid) newProjectEvents, newVersions)

ensurePhaseIsSetIfNecessary :: Project -> AppContextM [ProjectEvent]
ensurePhaseIsSetIfNecessary newProject = do
  uuid <- liftIO generateUuid
  mCurrentUser <- asks currentUser
  now <- liftIO getCurrentTime
  newProjectListEvents <- findProjectEventListsByProjectUuid newProject.uuid
  let projectContent = compileProjectEvents newProjectListEvents
  knowledgeModel <- compileKnowledgeModel [] (Just newProject.knowledgeModelPackageId) newProject.selectedQuestionTagUuids
  let newProjectEvents = fmap (toEvent newProject.uuid newProject.tenantUuid) newProjectListEvents
  return $
    case (headSafe knowledgeModel.phaseUuids, projectContent.phaseUuid) of
      (Nothing, Nothing) -> newProjectEvents
      (Nothing, Just projectPhaseUuid) -> newProjectEvents ++ [toProjectPhaseEvent uuid Nothing newProject.uuid newProject.tenantUuid mCurrentUser now]
      (Just kmPhaseUuid, Nothing) -> newProjectEvents ++ [toProjectPhaseEvent uuid (Just kmPhaseUuid) newProject.uuid newProject.tenantUuid mCurrentUser now]
      (Just kmPhaseUuid, Just projectPhaseUuid) ->
        if projectPhaseUuid `notElem` knowledgeModel.phaseUuids
          then newProjectEvents ++ [toProjectPhaseEvent uuid (Just kmPhaseUuid) newProject.uuid newProject.tenantUuid mCurrentUser now]
          else newProjectEvents

generateNewVersionUuid :: ProjectVersion -> AppContextM ProjectVersion
generateNewVersionUuid version = do
  newVersionUuid <- liftIO generateUuid
  return $ version {uuid = newVersionUuid}

getNewDocumentTemplateIdAndFormatUuid :: Project -> String -> AppContextM (Maybe String, Maybe U.UUID)
getNewDocumentTemplateIdAndFormatUuid oldProject newPkgId = do
  case oldProject.documentTemplateId of
    Just id -> do
      documentTemplate <- findDocumentTemplateById id
      if isPkgAllowedByDocumentTemplate newPkgId documentTemplate
        then return (Just id, oldProject.formatUuid)
        else return (Nothing, Nothing)
    Nothing -> return (Nothing, Nothing)
