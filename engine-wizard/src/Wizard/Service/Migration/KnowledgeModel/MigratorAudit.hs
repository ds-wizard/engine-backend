module Wizard.Service.Migration.KnowledgeModel.MigratorAudit where

import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.UUID as U

import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateCreateDTO
import Wizard.Model.Branch.Branch
import Wizard.Model.Context.AppContext
import Wizard.Service.Audit.AuditService

auditKmMigrationCreate :: MigratorStateCreateDTO -> Branch -> AppContextM ()
auditKmMigrationCreate reqDto branch =
  logAuditWithBody
    "knowledge_model.migration"
    "create"
    (U.toString branch.uuid)
    ( M.fromList
        [("sourcePackageId", fromMaybe "" $ branch.previousPackageId), ("targetPackageId", reqDto.targetPackageId)]
    )

auditKmMigrationSolve :: U.UUID -> MigratorConflictDTO -> AppContextM ()
auditKmMigrationSolve branchUuid reqDto =
  logAuditWithBody
    "knowledge_model.migration"
    "solve"
    (U.toString branchUuid)
    (M.fromList [("originalEventUuid", U.toString $ reqDto.originalEventUuid), ("action", show reqDto.action)])

auditKmMigrationApplyAll :: U.UUID -> AppContextM ()
auditKmMigrationApplyAll branchUuid = logAudit "knowledge_model.migration" "applyAll" (U.toString branchUuid)

auditKmMigrationCancel :: U.UUID -> AppContextM ()
auditKmMigrationCancel branchUuid = logAudit "knowledge_model.migration" "cancel" (U.toString branchUuid)

auditKmMigrationFinish :: U.UUID -> AppContextM ()
auditKmMigrationFinish branchUuid = logAudit "knowledge_model.migration" "finish" (U.toString branchUuid)
