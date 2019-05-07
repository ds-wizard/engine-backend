module Database.Migration.Production.Migration_0015_remove_cached_km.Migration
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
  { mmNumber = 15
  , mmName = "Remove cached Knowledge Model"
  , mmDescription =
      "Remove cached knowledge model from branches, questionnaires and migrations; Remove public questionnaire due to new public package"
  }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  deleteKMInBranches dbPool
  deleteKMInQuestionnaires dbPool
  deleteKMInKMMigrations dbPool
  deletePublicQuestionnaire dbPool
  return Nothing

deleteKMInBranches dbPool = do
  now <- liftIO getCurrentTime
  let action = modify (select [] "branches") ["$unset" =: ["knowledgeModel" =: ""], "$set" =: ["updatedAt" =: now]]
  runMongoDBPoolDef action dbPool
  return Nothing

deleteKMInQuestionnaires dbPool = do
  now <- liftIO getCurrentTime
  let action =
        modify (select [] "questionnaires") ["$unset" =: ["knowledgeModel" =: ""], "$set" =: ["updatedAt" =: now]]
  runMongoDBPoolDef action dbPool
  return Nothing

deleteKMInKMMigrations dbPool = do
  now <- liftIO getCurrentTime
  let action = modify (select [] "kmMigrations") ["$unset" =: ["currentKnowledgeModel" =: ""]]
  runMongoDBPoolDef action dbPool
  return Nothing

deletePublicQuestionnaire dbPool = do
  let action = dropCollection "publicQuestionnaire"
  runMongoDBPoolDef action dbPool
  return Nothing
