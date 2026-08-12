module Wizard.Service.Project.Migration.ProjectMigrationAudit where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Audit.Service.Audit.AuditService
import Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Project.Project

auditProjectMigration :: ProjectMigrationCreateDTO -> Project -> AppContextM ()
auditProjectMigration reqDto project =
  logAuditWithBody
    "project_migration"
    "migrate"
    (U.toString project.uuid)
    ( M.fromList
        [ ("sourceKnowledgeModelPackageUuid", U.toString project.knowledgeModelPackageUuid)
        , ("targetKnowledgeModelPackageUuid", U.toString reqDto.targetKnowledgeModelPackageUuid)
        ]
    )
