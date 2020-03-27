module Wizard.Database.Migration.Production.Migration_0026_add_application_configs.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Time
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

import Wizard.Database.Migration.Production.Migration_0026_add_application_configs.Data.Configs

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 26
    , mmName = "Add Application Configs"
    , mmDescription = "Add config into DB and add CFG_PERM to admin users"
    }

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  insertConfig dbPool
  addAffiliationToUsers dbPool
  addPermissionToAdmins dbPool
  return Nothing

insertConfig dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "appConfigs" (config now)
  runMongoDBPoolDef action dbPool

addAffiliationToUsers dbPool = do
  let action =
        modify
          (select [] "users")
          ["$set" =: ["affiliation" =: (Nothing :: Maybe String), "sources" =: ([] :: [String])]]
  runMongoDBPoolDef action dbPool

addPermissionToAdmins dbPool = do
  let action = modify (select ["role" =: "ADMIN"] "users") ["$push" =: ["permissions" =: "CFG_PERM"]]
  runMongoDBPoolDef action dbPool
