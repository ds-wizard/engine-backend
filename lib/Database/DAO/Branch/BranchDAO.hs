module Database.DAO.Branch.BranchDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Database.MongoDB
       ((=:), delete, deleteOne, fetch, find, findOne, insert, merge,
        modify, rest, save, select)

import Common.Error
import Database.BSON.Branch.Branch ()
import Database.BSON.Branch.BranchWithEvents ()
import Database.DAO.Common
import Model.Branch.Branch
import Model.Context.AppContext

branchCollection = "branches"

findBranches :: AppContextM (Either AppError [Branch])
findBranches = do
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

updateBranchById :: Branch -> AppContextM ()
updateBranchById branch = do
  let action =
        fetch (select ["uuid" =: (branch ^. bUuid)] branchCollection) >>= save branchCollection . merge (toBSON branch)
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
