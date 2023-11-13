module Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Migration.KnowledgeModel.MigratorState ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Migration.KnowledgeModel.MigratorState

entityName = "knowledge_model_migration"

pageLabel = "migrations"

findMigratorStates :: AppContextM [MigratorState]
findMigratorStates = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findMigratorStateByBranchUuid :: U.UUID -> AppContextM MigratorState
findMigratorStateByBranchUuid branchUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("branch_uuid", U.toString branchUuid)]

findMigratorStateByBranchUuid' :: U.UUID -> AppContextM (Maybe MigratorState)
findMigratorStateByBranchUuid' branchUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("branch_uuid", U.toString branchUuid)]

insertMigratorState :: MigratorState -> AppContextM Int64
insertMigratorState = createInsertFn entityName

updateMigratorState :: MigratorState -> AppContextM Int64
updateMigratorState ms = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE knowledge_model_migration SET branch_uuid = ?, metamodel_version = ?, migration_state = ?, branch_previous_package_id = ?, target_package_id = ?, branch_events = ?, target_package_events = ?, result_events = ?, current_knowledge_model = ?, tenant_uuid = ?, created_at = ? WHERE tenant_uuid = ? AND branch_uuid = ?"
  let params = toRow ms ++ [toField tenantUuid, toField ms.branchUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteMigratorStates :: AppContextM Int64
deleteMigratorStates = createDeleteEntitiesFn entityName

deleteMigratorStateByBranchUuid :: U.UUID -> AppContextM Int64
deleteMigratorStateByBranchUuid branchUuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("branch_uuid", U.toString branchUuid)]
