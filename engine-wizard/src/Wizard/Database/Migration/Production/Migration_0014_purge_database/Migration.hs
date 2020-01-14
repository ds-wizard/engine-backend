module Wizard.Database.Migration.Production.Migration_0014_purge_database.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
    {mmNumber = 14, mmName = "Purge database", mmDescription = "Purge all packages, branches and questionnaires"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  deleteQuestionnaires dbPool
  deleteBranches dbPool
  deletePackages dbPool
  return Nothing

deleteQuestionnaires dbPool = do
  let action = delete $ select [] "questionnaires"
  runMongoDBPoolDef action dbPool

deleteBranches dbPool = do
  let action = delete $ select [] "branches"
  runMongoDBPoolDef action dbPool

deletePackages dbPool = do
  let action = delete $ select [] "packages"
  runMongoDBPoolDef action dbPool
