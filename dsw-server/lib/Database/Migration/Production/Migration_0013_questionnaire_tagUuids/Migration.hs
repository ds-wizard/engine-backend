module Database.Migration.Production.Migration_0013_questionnaire_tagUuids.Migration
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
  {mmNumber = 13, mmName = "Questionnaire SelectedTagUuids", mmDescription = "Questionnaire can now have tags"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  now <- liftIO getCurrentTime
  let action =
        modify (select [] "questionnaires") ["$set" =: ["selectedTagUuids" =: ([] :: [String]), "updatedAt" =: now]]
  runMongoDBPoolDef action dbPool
  return Nothing
