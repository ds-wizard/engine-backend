module Wizard.Database.Migration.Production.Migration_0007_user_isActive.Migration
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
    {mmNumber = 7, mmName = "User IsActive", mmDescription = "Rename 'isActive' field to 'active' in User Entity"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  now <- liftIO getCurrentTime
  let action = modify (select [] "users") ["$rename" =: ["isActive" =: "active"], "$set" =: ["updatedAt" =: now]]
  runMongoDBPoolDef action dbPool
  return Nothing
