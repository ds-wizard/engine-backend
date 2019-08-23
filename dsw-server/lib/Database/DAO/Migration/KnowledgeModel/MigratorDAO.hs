module Database.DAO.Migration.KnowledgeModel.MigratorDAO where

import Control.Lens ((^.))
import Data.Bson

import Database.BSON.Migration.KnowledgeModel.MigratorState ()
import Database.DAO.Common
import LensesConfig
import Model.Context.AppContext
import Model.Error.Error
import Model.Migration.KnowledgeModel.MigratorState
import Util.Helper (createHeeHelper)

entityName = "kmMigration"

collection = "kmMigrations"

findMigratorStateByBranchUuid :: String -> AppContextM (Either AppError MigratorState)
findMigratorStateByBranchUuid = createFindEntityByFn collection entityName "branchUuid"

insertMigratorState :: MigratorState -> AppContextM Value
insertMigratorState = createInsertFn collection

updateMigratorState :: MigratorState -> AppContextM ()
updateMigratorState ms = createUpdateByFn collection "branchUuid" (ms ^. branchUuid) ms

deleteMigratorStates :: AppContextM ()
deleteMigratorStates = createDeleteEntitiesFn collection

deleteMigratorStateByBranchUuid :: String -> AppContextM ()
deleteMigratorStateByBranchUuid = createDeleteEntityByFn collection "branchUuid"

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindMigratorStateByBranchUuid branchUuid callback =
  createHeeHelper (findMigratorStateByBranchUuid branchUuid) callback
