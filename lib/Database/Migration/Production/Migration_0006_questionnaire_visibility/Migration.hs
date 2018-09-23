module Database.Migration.Production.Migration_0006_questionnaire_visibility.Migration
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
  {mmNumber = 6, mmName = "Questionnaire Visibility", mmDescription = "Questionnaire now can be private and public"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  now <- liftIO getCurrentTime
  let action =
        modify
          (select [] "questionnaires")
          ["$set" =: ["private" =: False, "ownerUuid" =: (Nothing :: Maybe Bool), "updatedAt" =: now]]
  runMongoDBPoolDef action dbPool
  return Nothing
