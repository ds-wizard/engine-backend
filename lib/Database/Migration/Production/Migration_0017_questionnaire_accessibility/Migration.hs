module Database.Migration.Production.Migration_0017_questionnaire_accessibility.Migration
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
  { mmNumber = 17
  , mmName = "Questionnaire Accessibility"
  , mmDescription = "Basic Questionnaire Visibility changed to more sophisticated Accessibility"
  }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  convertPrivateQuestionnaires dbPool
  convertPublicQuestionnaires dbPool

convertPrivateQuestionnaires dbPool = do
  now <- liftIO getCurrentTime
  let action =
        modify
          (select ["private" =: True] "questionnaires")
          ["$set" =: ["accessibility" =: "PrivateQuestionnaire", "updatedAt" =: now]]
  runMongoDBPoolDef action dbPool
  return Nothing

convertPublicQuestionnaires dbPool = do
  now <- liftIO getCurrentTime
  let action =
        modify
          (select ["private" =: False] "questionnaires")
          ["$set" =: ["accessibility" =: "PublicQuestionnaire", "updatedAt" =: now]]
  runMongoDBPoolDef action dbPool
  return Nothing
