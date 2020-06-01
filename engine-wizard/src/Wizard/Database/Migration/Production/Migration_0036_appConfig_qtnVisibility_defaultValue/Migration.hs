module Wizard.Database.Migration.Production.Migration_0036_appConfig_qtnVisibility_defaultValue.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 36
    , mmName = "AppConfig QtnVisibility - Default Value"
    , mmDescription = "Add 'defaultValue' to Qtn Visibility AppConfig"
    }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let action =
        modify
          (select [] "appConfigs")
          ["$set" =: ["questionnaire.questionnaireVisibility.defaultValue" =: "PrivateQuestionnaire"]]
  runMongoDBPoolDef action dbPool
  return Nothing
