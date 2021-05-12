module Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO where

import Control.Lens ((^.))
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Migration.KnowledgeModel.MigratorState ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Migration.KnowledgeModel.MigratorState

entityName = "km_migration"

pageLabel = "migrations"

findMigratorStates :: AppContextM [MigratorState]
findMigratorStates = createFindEntitiesFn entityName

findMigratorStateByBranchUuid :: String -> AppContextM MigratorState
findMigratorStateByBranchUuid = createFindEntityByFn entityName "branch_uuid"

findMigratorStateByBranchUuid' :: String -> AppContextM (Maybe MigratorState)
findMigratorStateByBranchUuid' = createFindEntityByFn' entityName "branch_uuid"

insertMigratorState :: MigratorState -> AppContextM Int64
insertMigratorState = createInsertFn entityName

updateMigratorState :: MigratorState -> AppContextM Int64
updateMigratorState ms = do
  let params = toRow ms ++ [toField . U.toText $ ms ^. branchUuid]
  let action conn =
        execute
          conn
          "UPDATE km_migration SET branch_uuid = ?, metamodel_version = ?, migration_state = ?, branch_previous_package_id = ?, target_package_id = ?, branch_events = ?, target_package_events = ?, result_events = ?, current_knowledge_model = ? WHERE branch_uuid = ?"
          params
  runDB action

deleteMigratorStates :: AppContextM Int64
deleteMigratorStates = createDeleteEntitiesFn entityName

deleteMigratorStateByBranchUuid :: String -> AppContextM Int64
deleteMigratorStateByBranchUuid = createDeleteEntityByFn entityName "branch_uuid"
