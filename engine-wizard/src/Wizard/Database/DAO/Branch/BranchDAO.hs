module Wizard.Database.DAO.Branch.BranchDAO where

import Control.Lens ((^.))
import Data.Bson
import Database.MongoDB ((=:))

import LensesConfig
import Shared.Model.Error.Error
import Shared.Util.Helper (createHeeHelper, createHemHelper)
import Wizard.Database.BSON.Branch.Branch ()
import Wizard.Database.BSON.Branch.BranchWithEvents ()
import Wizard.Database.DAO.Common
import Wizard.Model.Branch.Branch
import Wizard.Model.Context.AppContext

entityName = "branch"

collection = "branches"

findBranches :: AppContextM (Either AppError [Branch])
findBranches = createFindEntitiesFn collection

findBranchesByPreviousPackageId :: String -> AppContextM (Either AppError [Branch])
findBranchesByPreviousPackageId previousPackageId =
  createFindEntitiesByFn collection ["previousPackageId" =: previousPackageId]

findBranchesWithEvents :: AppContextM (Either AppError [BranchWithEvents])
findBranchesWithEvents = createFindEntitiesFn collection

findBranchById :: String -> AppContextM (Either AppError Branch)
findBranchById = createFindEntityByFn collection entityName "uuid"

findBranchByKmId :: String -> AppContextM (Either AppError Branch)
findBranchByKmId = createFindEntityByFn collection entityName "kmId"

findBranchWithEventsById :: String -> AppContextM (Either AppError BranchWithEvents)
findBranchWithEventsById = createFindEntityByFn collection entityName "uuid"

insertBranch :: BranchWithEvents -> AppContextM Value
insertBranch = createInsertFn collection

updateBranchById :: BranchWithEvents -> AppContextM ()
updateBranchById branch = createUpdateByFn collection "uuid" (branch ^. uuid) branch

updateBranchWithMigrationInfo :: String -> String -> String -> AppContextM ()
updateBranchWithMigrationInfo branchUuid forkOfPackageId mergeCheckpointPackageId =
  createPartialUpdateByFn
    collection
    ["uuid" =: branchUuid]
    ["forkOfPackageId" =: forkOfPackageId, "mergeCheckpointPackageId" =: mergeCheckpointPackageId]

updateBranchWithPreviousPackageId :: String -> String -> AppContextM ()
updateBranchWithPreviousPackageId branchUuid previousPackageId =
  createPartialUpdateByFn' collection "uuid" branchUuid "previousPackageId" previousPackageId

deleteBranches :: AppContextM ()
deleteBranches = createDeleteEntitiesFn collection

deleteBranchById :: String -> AppContextM ()
deleteBranchById = createDeleteEntityByFn collection "uuid"

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindBranches callback = createHeeHelper findBranches callback

-- -----------------------------------------------------
heFindBranchesWithEvents callback = createHeeHelper findBranchesWithEvents callback

-- -----------------------------------------------------
heFindBranchById branchUuid callback = createHeeHelper (findBranchById branchUuid) callback

hmFindBranchById branchUuid callback = createHemHelper (findBranchById branchUuid) callback

-- -----------------------------------------------------
heFindBranchWithEventsById branchUuid callback = createHeeHelper (findBranchWithEventsById branchUuid) callback
