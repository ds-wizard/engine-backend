module Wizard.Service.Project.Migration.ProjectMigrationAudit where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Audit.Service.Audit.AuditService
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationChangeDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Project.Project

auditProjectMigrationCreate :: ProjectMigrationCreateDTO -> Project -> Project -> AppContextM ()
auditProjectMigrationCreate reqDto oldProject newProject =
  logAuditWithBody
    "project_migration"
    "create"
    (U.toString oldProject.uuid)
    ( M.fromList
        [ ("sourceKnowledgeModelPackageId", oldProject.knowledgeModelPackageId)
        , ("targetKnowledgeModelPackageId", reqDto.targetKnowledgeModelPackageId)
        , ("oldProjectUuid", U.toString $ oldProject.uuid)
        , ("newProjectUuid", U.toString $ newProject.uuid)
        ]
    )

auditProjectMigrationModify :: ProjectMigrationDTO -> ProjectMigrationChangeDTO -> AppContextM ()
auditProjectMigrationModify projectMigration resolvedQuestionUuids =
  logAuditWithBody
    "project_migration"
    "modify"
    (U.toString $ projectMigration.newProject.uuid)
    (M.fromList [("resolvedQuestionUuids", show resolvedQuestionUuids)])

auditProjectMigrationFinish :: Project -> Project -> AppContextM ()
auditProjectMigrationFinish oldProject newProject =
  logAuditWithBody
    "project_migration"
    "finish"
    (U.toString oldProject.uuid)
    ( M.fromList
        [("oldProjectUuid", U.toString $ oldProject.uuid), ("newProjectUuid", U.toString $ newProject.uuid)]
    )

auditProjectMigrationCancel :: ProjectMigrationDTO -> AppContextM ()
auditProjectMigrationCancel projectMigration =
  logAudit "project_migration" "cancel" (U.toString $ projectMigration.oldProject.uuid)
