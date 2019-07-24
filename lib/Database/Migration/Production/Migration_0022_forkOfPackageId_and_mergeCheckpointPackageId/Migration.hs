module Database.Migration.Production.Migration_0022_forkOfPackageId_and_mergeCheckpointPackageId.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
  { mmNumber = 22
  , mmName = "ForkOfPackageId and MergeCheckpointPackageId"
  , mmDescription = "Rename and move to package - ForkOfPackageId and MergeCheckpointPackageId"
  }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  migrateBranches dbPool
  migrateKmMigrations dbPool
  migratePackages dbPool

migrateBranches dbPool = do
  let action =
        modify
          (select [] "branches")
          [ "$rename" =: ["parentPackageId" =: "previousPackageId"]
          , "$unset" =: ["lastAppliedParentPackageId" =: "", "lastMergeCheckpointPackageId" =: ""]
          ]
  runMongoDBPoolDef action dbPool
  return Nothing

migrateKmMigrations dbPool = do
  let action = modify (select [] "kmMigrations") ["$rename" =: ["branchParentId" =: "branchPreviousPackageId"]]
  runMongoDBPoolDef action dbPool
  return Nothing

migratePackages dbPool = do
  let action =
        modify
          (select [] "packages")
          [ "$rename" =: ["parentPackageId" =: "previousPackageId"]
          , "$set" =: ["forkOfPackageId" =: ([] :: [String]), "mergeCheckpointPackageId" =: ([] :: [String])]
          ]
  runMongoDBPoolDef action dbPool
  return Nothing
