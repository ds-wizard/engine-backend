module Database.DAO.Branch.BranchDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Database.MongoDB
       ((=:), delete, deleteOne, fetch, find, findOne, insert, merge,
        modify, rest, save, select)

import Database.BSON.Branch.Branch ()
import Database.BSON.Branch.BranchWithEvents ()
import Database.DAO.Common
import LensesConfig
import Model.Branch.Branch
import Model.Context.AppContext
import Model.Error.Error

branchCollection = "branches"

findBranches :: AppContextM (Either AppError [Branch])
findBranches = do
  let action = rest =<< find (select [] branchCollection)
  branchesS <- runDB action
  return . deserializeEntities $ branchesS

findBranchesWithEvents :: AppContextM (Either AppError [BranchWithEvents])
findBranchesWithEvents = do
  let action = rest =<< find (select [] branchCollection)
  branchesS <- runDB action
  return . deserializeEntities $ branchesS

findBranchById :: String -> AppContextM (Either AppError Branch)
findBranchById branchUuid = do
  let action = findOne $ select ["uuid" =: branchUuid] branchCollection
  maybeBranchS <- runDB action
  return . deserializeMaybeEntity $ maybeBranchS

findBranchByKmId :: String -> AppContextM (Either AppError Branch)
findBranchByKmId kmId = do
  let action = findOne $ select ["kmId" =: kmId] branchCollection
  maybeBranchS <- runDB action
  return . deserializeMaybeEntity $ maybeBranchS

findBranchWithEventsById :: String -> AppContextM (Either AppError BranchWithEvents)
findBranchWithEventsById branchUuid = do
  let action = findOne $ select ["uuid" =: branchUuid] branchCollection
  maybeBranchWithEventsS <- runDB action
  return . deserializeMaybeEntity $ maybeBranchWithEventsS

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
             branchCollection)
  branchesS <- runDB action
  return . deserializeEntities $ branchesS

insertBranch :: Branch -> AppContextM Value
insertBranch branch = do
  let action = insert branchCollection (toBSON branch)
  runDB action

updateBranchById :: BranchWithEvents -> AppContextM ()
updateBranchById branch = do
  let action =
        fetch (select ["uuid" =: (branch ^. uuid)] branchCollection) >>= save branchCollection . merge (toBSON branch)
  runDB action

updateBranchWithMigrationInfo :: String -> String -> String -> AppContextM ()
updateBranchWithMigrationInfo branchUuid lastAppliedParentPackageId lastMergeCheckpointPackageId = do
  let action =
        modify
          (select ["uuid" =: branchUuid] branchCollection)
          [ "$set" =:
            [ "lastAppliedParentPackageId" =: lastAppliedParentPackageId
            , "lastMergeCheckpointPackageId" =: lastMergeCheckpointPackageId
            ]
          ]
  runDB action

updateBranchWithParentPackageId :: String -> String -> AppContextM ()
updateBranchWithParentPackageId branchUuid parentPackageId = do
  let action =
        modify (select ["uuid" =: branchUuid] branchCollection) ["$set" =: ["parentPackageId" =: parentPackageId]]
  runDB action

deleteBranches :: AppContextM ()
deleteBranches = do
  let action = delete $ select [] branchCollection
  runDB action

deleteBranchById :: String -> AppContextM ()
deleteBranchById branchUuid = do
  let action = deleteOne $ select ["uuid" =: branchUuid] branchCollection
  runDB action

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindBranches callback = do
  eitherBranches <- findBranches
  case eitherBranches of
    Right branches -> callback branches
    Left error -> return . Left $ error

-- -----------------------------------------------------
heFindBranchesWithEvents callback = do
  eitherBranches <- findBranchesWithEvents
  case eitherBranches of
    Right branches -> callback branches
    Left error -> return . Left $ error

-- -----------------------------------------------------
heFindBranchById branchUuid callback = do
  eitherBranch <- findBranchById branchUuid
  case eitherBranch of
    Right branch -> callback branch
    Left error -> return . Left $ error

hmFindBranchById branchUuid callback = do
  eitherBranch <- findBranchById branchUuid
  case eitherBranch of
    Right branch -> callback branch
    Left error -> return . Just $ error

-- -----------------------------------------------------
heFindBranchWithEventsById branchUuid callback = do
  eitherBranch <- findBranchWithEventsById branchUuid
  case eitherBranch of
    Right branch -> callback branch
    Left error -> return . Left $ error
