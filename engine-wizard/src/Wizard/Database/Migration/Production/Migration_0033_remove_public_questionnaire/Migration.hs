module Wizard.Database.Migration.Production.Migration_0033_remove_public_questionnaire.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Database.MongoDB hiding (createIndex)
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 33
    , mmName = "Remove public questionnaire"
    , mmDescription = "Remove publicPackages coll and publicQuestionnaire from AppConfig"
    }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  dropPublicPackagesCollection dbPool
  removePublicPackagesFromAppConfig dbPool

dropPublicPackagesCollection dbPool = do
  let action = dropCollection "publicPackages"
  runMongoDBPoolDef action dbPool
  return Nothing

removePublicPackagesFromAppConfig dbPool = do
  let action = modify (select [] "appConfigs") ["$unset" =: ["questionnaire.publicQuestionnaire" =: ""]]
  runMongoDBPoolDef action dbPool
  return Nothing
