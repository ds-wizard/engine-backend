module Wizard.Database.Migration.Production.Migration_0011_update_data_steward_perms.Migration
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
    { mmNumber = 11
    , mmName = "Update DATASTEWARD Perms"
    , mmDescription = "Add 'PM_WRITE_PERM' to DATASTEWARD permissions"
    }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  now <- liftIO getCurrentTime
  let action =
        modify
          (select ["role" =: "DATASTEWARD"] "users")
          ["$push" =: ["permissions" =: "PM_WRITE_PERM"], "$set" =: ["updatedAt" =: now]]
  runMongoDBPoolDef action dbPool
  return Nothing
