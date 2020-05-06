module Wizard.Database.Migration.Production.Migration_0032_appConfig_organizationDescription.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 32
    , mmName = "AppConfig Organization Description"
    , mmDescription = "Add 'description' to AppConfig Organization"
    }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let action = modify (select [] "appConfigs") ["$set" =: ["organization.description" =: ""]]
  runMongoDBPoolDef action dbPool
  return Nothing
