module Wizard.Service.Project.Migration.ProjectMigrationService where

import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.List
import Shared.Common.Util.Uuid
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO (findPackageByUuid)
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Event.ProjectEvent
import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectContent
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Service.Project.Collaboration.ProjectCollaborationService
import Wizard.Service.Project.Compiler.ProjectCompilerService
import Wizard.Service.Project.Event.ProjectEventMapper
import Wizard.Service.Project.Migration.Migrator.Sanitizer
import Wizard.Service.Project.Migration.ProjectMigrationAudit
import Wizard.Service.Project.Migration.ProjectMigrationMapper
import Wizard.Service.Project.ProjectAcl
import Wizard.Service.Project.ProjectService

migrateProject :: U.UUID -> ProjectMigrationCreateDTO -> AppContextM ProjectDetailQuestionnaireDTO
migrateProject projectUuid reqDto =
  runInTransaction $ do
    project <- findProjectByUuid projectUuid
    checkMigrationPermissionToProject project.visibility project.permissions
    newPkg <- findPackageByUuid reqDto.targetKnowledgeModelPackageUuid
    oldKm <- compileKnowledgeModel [] (Just project.knowledgeModelPackageUuid) reqDto.targetTagUuids
    newKm <- compileKnowledgeModel [] (Just reqDto.targetKnowledgeModelPackageUuid) reqDto.targetTagUuids
    projectEvents <- findProjectEventListsByProjectUuid project.uuid
    deltaEvents <- sanitizeProjectEvents oldKm newKm projectEvents
    phaseEvents <- ensurePhaseIsSetIfNecessary project newKm projectEvents
    (newDocumentTemplateUuid, newFormatUuid) <- getNewDocumentTemplateIdAndFormatUuid project newPkg
    now <- liftIO getCurrentTime
    let updatedProject =
          project
            { knowledgeModelPackageUuid = reqDto.targetKnowledgeModelPackageUuid
            , selectedQuestionTagUuids = reqDto.targetTagUuids
            , documentTemplateUuid = newDocumentTemplateUuid
            , formatUuid = newFormatUuid
            , squashed = False
            , updatedAt = now
            }
            :: Project
    updateProjectByUuid updatedProject
    insertProjectEvents (fmap (toEvent project.uuid project.tenantUuid) deltaEvents ++ phaseEvents)
    auditProjectMigration reqDto project
    logOutOnlineUsersWhenProjectDramaticallyChanged project.uuid
    getProjectDetailQuestionnaireByUuid project.uuid

-- --------------------------------
-- PRIVATE
-- --------------------------------
ensurePhaseIsSetIfNecessary :: Project -> KnowledgeModel -> [ProjectEventList] -> AppContextM [ProjectEvent]
ensurePhaseIsSetIfNecessary project newKm projectEvents = do
  uuid <- liftIO generateUuid
  mCurrentUser <- asks currentUser
  now <- liftIO getCurrentTime
  let projectContent = compileProjectEvents projectEvents
  return $
    case (headSafe newKm.phaseUuids, projectContent.phaseUuid) of
      (Nothing, Nothing) -> []
      (Nothing, Just projectPhaseUuid) -> [toProjectPhaseEvent uuid Nothing project.uuid project.tenantUuid mCurrentUser now]
      (Just kmPhaseUuid, Nothing) -> [toProjectPhaseEvent uuid (Just kmPhaseUuid) project.uuid project.tenantUuid mCurrentUser now]
      (Just kmPhaseUuid, Just projectPhaseUuid)
        | projectPhaseUuid `notElem` newKm.phaseUuids -> [toProjectPhaseEvent uuid (Just kmPhaseUuid) project.uuid project.tenantUuid mCurrentUser now]
        | otherwise -> []

getNewDocumentTemplateIdAndFormatUuid :: Project -> KnowledgeModelPackage -> AppContextM (Maybe U.UUID, Maybe U.UUID)
getNewDocumentTemplateIdAndFormatUuid oldProject newPkg = do
  case oldProject.documentTemplateUuid of
    Just dtUuid -> do
      documentTemplate <- findDocumentTemplateByUuid dtUuid
      if fitsIntoKMSpecs (createCoordinate newPkg) documentTemplate.allowedPackages
        then return (Just dtUuid, oldProject.formatUuid)
        else return (Nothing, Nothing)
    Nothing -> return (Nothing, Nothing)
