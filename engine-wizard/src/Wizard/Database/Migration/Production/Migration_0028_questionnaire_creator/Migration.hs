module Wizard.Database.Migration.Production.Migration_0028_questionnaire_creator.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 28, mmName = "Questionnaire Creator", mmDescription = "Add creator to questionnaire"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let action = modify (select [] "questionnaires") ["$set" =: ["creatorUuid" =: (Nothing :: Maybe String)]]
  runMongoDBPoolDef action dbPool
  return Nothing
