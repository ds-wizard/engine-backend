module Wizard.Database.Migration.Production.Migration_0037_questionnaireSharing.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 37, mmName = "Questionnaire Sharing", mmDescription = "Add questionnaire 'sharing'"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addDefaultSharingToQuestionnaires dbPool
  addSharingToAppConfig dbPool
  return Nothing

addDefaultSharingToQuestionnaires dbPool = do
  let action = modify (select [] "questionnaires") ["$set" =: ["sharing" =: "RestrictedQuestionnaire"]]
  runMongoDBPoolDef action dbPool
  return Nothing

addSharingToAppConfig dbPool = do
  let action =
        modify
          (select [] "appConfigs")
          [ "$set" =:
            [ "questionnaire.questionnaireSharing" =:
              ["enabled" =: True, "defaultValue" =: "RestrictedQuestionnaire"]
            ]
          ]
  runMongoDBPoolDef action dbPool
