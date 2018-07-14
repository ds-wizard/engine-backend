module Database.Migration.Production.Migration_0002_users_init.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Time
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

import Database.Migration.Production.Migration_0002_users_init.Data.Users

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 2, mmName = "Users Init", mmDescription = ""}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  insertAlbert dbPool
  insertNikola dbPool
  insertIsaac dbPool
  return Nothing

insertAlbert dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "users" (userAlbert now)
  runMongoDBPoolDef action dbPool

insertNikola dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "users" (userNikola now)
  runMongoDBPoolDef action dbPool

insertIsaac dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "users" (userIsaac now)
  runMongoDBPoolDef action dbPool
