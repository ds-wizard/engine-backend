module Wizard.Database.Migration.Production.Migration_0034_appConfig_recommendedTemplateUuid.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 34
    , mmName = "AppConfig Recommended TemplateUuid"
    , mmDescription = "Add 'recommendedTemplateUuid' to AppConfig"
    }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let action =
        modify (select [] "appConfigs") ["$set" =: ["template.recommendedTemplateUuid" =: (Nothing :: Maybe String)]]
  runMongoDBPoolDef action dbPool
  return Nothing
