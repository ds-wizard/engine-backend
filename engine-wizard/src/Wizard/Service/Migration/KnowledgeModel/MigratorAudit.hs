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
    (U.toString $ branch.uuid)
    ( M.fromList
        [("sourcePackageId", fromMaybe "" $ branch.previousPackageId), ("targetPackageId", reqDto.targetPackageId)]
    )

auditKmMigrationSolve :: String -> MigratorConflictDTO -> AppContextM ()
auditKmMigrationSolve branchUuid reqDto =
  logAuditWithBody
    "knowledge_model.migration"
    "solve"
    branchUuid
    (M.fromList [("originalEventUuid", U.toString $ reqDto.originalEventUuid), ("action", show reqDto.action)])

auditKmMigrationApplyAll :: String -> AppContextM ()
auditKmMigrationApplyAll = logAudit "knowledge_model.migration" "applyAll"

auditKmMigrationCancel :: String -> AppContextM ()
auditKmMigrationCancel = logAudit "knowledge_model.migration" "cancel"

auditKmMigrationFinish :: String -> AppContextM ()
auditKmMigrationFinish = logAudit "knowledge_model.migration" "finish"
