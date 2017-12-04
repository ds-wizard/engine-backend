module Database.DAO.Branch.BranchDAO where

import Control.Lens ((^.))
import Data.Bson
import Data.Bson.Generic
import Data.Maybe
import Database.MongoDB
       ((=:), delete, deleteOne, fetch, find, findOne, insert, merge,
        modify, rest, save, select)
import Database.Persist.MongoDB (runMongoDBPoolDef)

import Common.Context
import Common.Error
import Common.Types
import Database.BSON.Branch.Branch
import Database.BSON.Branch.BranchWithEvents
import Database.DAO.Common
import Model.Branch.Branch

branchCollection = "branches"

findBranches :: Context -> IO (Either AppError [Branch])
findBranches context = do
  let action = rest =<< find (select [] branchCollection)
  branchesS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeEntities $ branchesS

findBranchById :: Context -> String -> IO (Either AppError Branch)
findBranchById context branchUuid = do
  let action = findOne $ select ["uuid" =: branchUuid] branchCollection
  maybeBranchS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeBranchS

findBranchByArtifactId :: Context -> String -> IO (Either AppError Branch)
findBranchByArtifactId context artifactId = do
  let action = findOne $ select ["artifactId" =: artifactId] branchCollection
  maybeBranchS <- runMongoDBPoolDef action (context ^. ctxDbPool)
  return . deserializeMaybeEntity $ maybeBranchS

insertBranch :: Context -> Branch -> IO Value
insertBranch context branch = do
  let action = insert branchCollection (toBSON branch)
  runMongoDBPoolDef action (context ^. ctxDbPool)

updateBranchById :: Context -> Branch -> IO ()
updateBranchById context branch = do
  let action =
        fetch (select ["uuid" =: (branch ^. bUuid)] branchCollection) >>= save branchCollection . merge (toBSON branch)
  runMongoDBPoolDef action (context ^. ctxDbPool)

updateBranchWithMigrationInfo :: Context -> String -> String -> String -> IO ()
updateBranchWithMigrationInfo context branchUuid lastAppliedParentPackageId lastMergeCheckpointPackageId = do
  let action =
        modify
          (select ["uuid" =: branchUuid] branchCollection)
          [ "$set" =:
            [ "lastAppliedParentPackageId" =: lastAppliedParentPackageId
            , "lastMergeCheckpointPackageId" =: lastMergeCheckpointPackageId
            ]
          ]
  runMongoDBPoolDef action (context ^. ctxDbPool)

updateBranchWithParentPackageId :: Context -> String -> String -> IO ()
updateBranchWithParentPackageId context branchUuid parentPackageId = do
  let action =
        modify (select ["uuid" =: branchUuid] branchCollection) ["$set" =: ["parentPackageId" =: parentPackageId]]
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteBranches :: Context -> IO ()
deleteBranches context = do
  let action = delete $ select [] branchCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)

deleteBranchById :: Context -> String -> IO ()
deleteBranchById context branchUuid = do
  let action = deleteOne $ select ["uuid" =: branchUuid] branchCollection
  runMongoDBPoolDef action (context ^. ctxDbPool)
