module Database.Migration.Production.Migration_0005_levels_init.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Time
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

import Database.Migration.Production.Migration_0005_levels_init.Data.Levels

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 5, mmName = "Levels Init", mmDescription = ""}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  insert1 dbPool
  insert2 dbPool
  insert3 dbPool
  return Nothing

insert1 dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "levels" (level1 now)
  runMongoDBPoolDef action dbPool

insert2 dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "levels" (level2 now)
  runMongoDBPoolDef action dbPool

insert3 dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "levels" (level3 now)
  runMongoDBPoolDef action dbPool
