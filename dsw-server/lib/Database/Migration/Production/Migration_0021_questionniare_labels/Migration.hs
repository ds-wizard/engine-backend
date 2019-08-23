module Database.Migration.Production.Migration_0021_questionniare_labels.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta {mmNumber = 21, mmName = "Questionnaire Labels", mmDescription = "Add labels field to questionnaire"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let action = modify (select [] "questionnaires") ["$set" =: ["labels" =: ([] :: [String])]]
  runMongoDBPoolDef action dbPool
  return Nothing
