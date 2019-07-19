module Database.DAO.Branch.BranchDAO where

import Control.Lens ((^.))
import Data.Bson
import Database.MongoDB ((=:), find, rest, select)

import Database.BSON.Branch.Branch ()
import Database.BSON.Branch.BranchWithEvents ()
import Database.DAO.Common
import LensesConfig
import Model.Branch.Branch
import Model.Context.AppContext
import Model.Error.Error
import Util.Helper (createHeeHelper, createHemHelper)

entityName = "branch"

collection = "branches"

findBranches :: AppContextM (Either AppError [Branch])
findBranches = createFindEntitiesFn collection

findBranchesWithEvents :: AppContextM (Either AppError [BranchWithEvents])
findBranchesWithEvents = createFindEntitiesFn collection

findBranchById :: String -> AppContextM (Either AppError Branch)
findBranchById = createFindEntityByFn collection entityName "uuid"

findBranchByKmId :: String -> AppContextM (Either AppError Branch)
findBranchByKmId = createFindEntityByFn collection entityName "kmId"

findBranchWithEventsById :: String -> AppContextM (Either AppError BranchWithEvents)
findBranchWithEventsById = createFindEntityByFn collection entityName "uuid"

findBranchByParentPackageIdOrLastAppliedParentPackageIdOrLastMergeCheckpointPackageId ::
     String -> AppContextM (Either AppError [Branch])
findBranchByParentPackageIdOrLastAppliedParentPackageIdOrLastMergeCheckpointPackageId packageId = do
  let action =
        rest =<<
        find
          (select
             [ "$or" =:
               [ ["parentPackageId" =: packageId]
               , ["lastAppliedParentPackageId" =: packageId]
               , ["lastMergeCheckpointPackageId" =: packageId]
               ]
             ]
             collection)
  branchesS <- runDB action
  return . deserializeEntities $ branchesS

insertBranch :: BranchWithEvents -> AppContextM Value
insertBranch = createInsertFn collection

updateBranchById :: BranchWithEvents -> AppContextM ()
updateBranchById branch = createUpdateByFn collection "uuid" (branch ^. uuid) branch

updateBranchWithMigrationInfo :: String -> String -> String -> AppContextM ()
updateBranchWithMigrationInfo branchUuid lastAppliedParentPackageId lastMergeCheckpointPackageId =
  createPartialUpdateByFn
    collection
    ["uuid" =: branchUuid]
    [ "lastAppliedParentPackageId" =: lastAppliedParentPackageId
    , "lastMergeCheckpointPackageId" =: lastMergeCheckpointPackageId
    ]

updateBranchWithParentPackageId :: String -> String -> AppContextM ()
updateBranchWithParentPackageId branchUuid parentPackageId =
  createPartialUpdateByFn' collection "uuid" branchUuid "parentPackageId" parentPackageId

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
