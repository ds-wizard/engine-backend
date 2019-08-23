module Database.Migration.Production.Migration_0010_branch_owner_and_timestamp.Migration
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
  { mmNumber = 10
  , mmName = "Branch Owner and Timestamps"
  , mmDescription = "Knowledge Model in Editor has its owner and timestamps"
  }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  now <- liftIO getCurrentTime
  let action =
        modify
          (select [] "branches")
          ["$set" =: ["ownerUuid" =: (Nothing :: Maybe Bool), "createdAt" =: now, "updatedAt" =: now]]
  runMongoDBPoolDef action dbPool
  return Nothing
