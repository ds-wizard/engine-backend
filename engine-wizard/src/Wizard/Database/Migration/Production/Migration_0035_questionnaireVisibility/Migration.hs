module Wizard.Database.Migration.Production.Migration_0035_questionnaireVisibility.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 35
    , mmName = "Questionnaire Visibility"
    , mmDescription = "Rename questionnaire 'accesibility' to 'visibility'"
    }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  renameQuestionnaire dbPool
  renameAppConfig dbPool
  return Nothing

renameQuestionnaire dbPool = do
  let action = modify (select [] "questionnaires") ["$rename" =: ["accessibility" =: "visibility"]]
  runMongoDBPoolDef action dbPool

renameAppConfig dbPool = do
  let action =
        modify
          (select [] "appConfigs")
          ["$rename" =: ["questionnaire.questionnaireAccessibility" =: "questionnaire.questionnaireVisibility"]]
  runMongoDBPoolDef action dbPool
