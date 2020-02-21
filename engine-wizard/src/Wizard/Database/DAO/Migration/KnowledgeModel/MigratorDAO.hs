module Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO where

import Control.Lens ((^.))
import Data.Bson

import LensesConfig
import Wizard.Database.BSON.Migration.KnowledgeModel.MigratorState ()
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Migration.KnowledgeModel.MigratorState

entityName = "kmMigration"

collection = "kmMigrations"

findMigratorStateByBranchUuid :: String -> AppContextM MigratorState
findMigratorStateByBranchUuid = createFindEntityByFn collection entityName "branchUuid"

findMigratorStateByBranchUuid' :: String -> AppContextM (Maybe MigratorState)
findMigratorStateByBranchUuid' = createFindEntityByFn' collection entityName "branchUuid"

insertMigratorState :: MigratorState -> AppContextM Value
insertMigratorState = createInsertFn collection

updateMigratorState :: MigratorState -> AppContextM ()
updateMigratorState ms = createUpdateByFn collection "branchUuid" (ms ^. branchUuid) ms

deleteMigratorStates :: AppContextM ()
deleteMigratorStates = createDeleteEntitiesFn collection

deleteMigratorStateByBranchUuid :: String -> AppContextM ()
deleteMigratorStateByBranchUuid = createDeleteEntityByFn collection "branchUuid"
