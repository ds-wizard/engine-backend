module Database.Migration.Production.Migration_0016_metamodel_version.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Time
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
  {mmNumber = 16, mmName = "Metamodel Version", mmDescription = "Add metamodel version to branch and kmMigrations"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  updateBranches dbPool
  updateKMMigrations dbPool
  return Nothing

updateBranches dbPool = do
  now <- liftIO getCurrentTime
  let action = modify (select [] "branches") ["$set" =: ["metamodelVersion" =: 1], "$set" =: ["updatedAt" =: now]]
  runMongoDBPoolDef action dbPool
  return Nothing

updateKMMigrations dbPool = do
  let action = modify (select [] "kmMigrations") ["$set" =: ["metamodelVersion" =: 1]]
  runMongoDBPoolDef action dbPool
  return Nothing
