module Wizard.Database.Migration.Production.Migration_0040_user_lastVisitedAt.Migration
  ( definition
  ) where

import Control.Monad.Logger hiding (logInfo)
import Control.Monad.Reader (liftIO)
import Data.Time
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 40, mmName = "User LastVisitedAt", mmDescription = "Add lastVisitedAt timestamp"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  now <- liftIO getCurrentTime
  let action = modify (select [] "users") ["$set" =: ["lastVisitedAt" =: now]]
  runMongoDBPoolDef action dbPool
  return Nothing
