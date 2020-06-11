module Wizard.Database.DAO.Branch.BranchDAO where

import Control.Lens ((^.))
import Data.Bson

import LensesConfig
import Shared.Database.DAO.Common
import Wizard.Database.BSON.Branch.Branch ()
import Wizard.Database.BSON.Branch.BranchWithEvents ()
import Wizard.Model.Branch.Branch
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextLenses ()

entityName = "branch"

collection = "branches"

findBranches :: AppContextM [Branch]
findBranches = createFindEntitiesFn collection

findBranchesByPreviousPackageId :: String -> AppContextM [Branch]
findBranchesByPreviousPackageId previousPackageId =
  createFindEntitiesByFn collection ["previousPackageId" =: previousPackageId]

findBranchesWithEvents :: AppContextM [BranchWithEvents]
findBranchesWithEvents = createFindEntitiesFn collection

findBranchById :: String -> AppContextM Branch
findBranchById = createFindEntityByFn collection entityName "uuid"

findBranchByKmId :: String -> AppContextM Branch
findBranchByKmId = createFindEntityByFn collection entityName "kmId"

findBranchByKmId' :: String -> AppContextM (Maybe Branch)
findBranchByKmId' = createFindEntityByFn' collection entityName "kmId"

findBranchWithEventsById :: String -> AppContextM BranchWithEvents
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
updateBranchWithPreviousPackageId branchUuid = createPartialUpdateByFn' collection "uuid" branchUuid "previousPackageId"

deleteBranches :: AppContextM ()
deleteBranches = createDeleteEntitiesFn collection

deleteBranchById :: String -> AppContextM ()
deleteBranchById = createDeleteEntityByFn collection "uuid"
