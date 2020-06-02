module Wizard.Database.Migration.Production.Migration_0037_appConfig_qtn_summaryReport.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 37
    , mmName = "AppConfig QtnVisibility - Summary Report"
    , mmDescription = "Add 'summaryReport' to Qtn AppConfig"
    }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let action = modify (select [] "appConfigs") ["$set" =: ["questionnaire.summaryReport.enabled" =: True]]
  runMongoDBPoolDef action dbPool
  return Nothing
