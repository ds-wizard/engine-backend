module Database.Migration.Production.Migration_0012_erase_questionnaire_replies.Migration
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
  { mmNumber = 12
  , mmName = "Erase questionnaire replies"
  , mmDescription = "Due to change replies structure, it's necessary to erase all save replies"
  }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  now <- liftIO getCurrentTime
  let action = modify (select [] "questionnaires") ["$set" =: ["replies" =: ([] :: [String]), "updatedAt" =: now]]
  runMongoDBPoolDef action dbPool
  return Nothing
