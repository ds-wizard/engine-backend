module Wizard.Database.Migration.Production.Migration_0024_user_name_and_surname.Migration
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
    { mmNumber = 24
    , mmName = "User name and surname"
    , mmDescription = "Rename name and surname to firstName and lastName"
    }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  now <- liftIO getCurrentTime
  let action =
        modify
          (select [] "users")
          ["$rename" =: ["name" =: "firstName", "surname" =: "lastName"], "$set" =: ["updatedAt" =: now]]
  runMongoDBPoolDef action dbPool
  return Nothing
