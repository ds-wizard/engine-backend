module Wizard.Database.Migration.Production.Migration_0026_add_application_configs.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Maybe (fromJust)
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
  changeRoles dbPool
  addPermissionToAdmins dbPool
  moveOrganizationToConfig dbPool
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

changeRoles dbPool = do
  changeRole dbPool "ADMIN" "admin"
  changeRole dbPool "DATASTEWARD" "dataSteward"
  changeRole dbPool "RESEARCHER" "researcher"
  let action = modify (select ["role" =: "ADMIN"] "users") ["$set" =: ["affiliation" =: "admin"]]
  runMongoDBPoolDef action dbPool

changeRole dbPool oldRole newRole = do
  let action = modify (select ["role" =: oldRole] "users") ["$set" =: ["role" =: newRole]]
  runMongoDBPoolDef action dbPool

addPermissionToAdmins dbPool = do
  let action = modify (select ["role" =: "admin"] "users") ["$push" =: ["permissions" =: "CFG_PERM"]]
  runMongoDBPoolDef action dbPool

moveOrganizationToConfig dbPool
  -- Get organization
 = do
  let actionSelect = findOne (select [] "organizations")
  organization <- runMongoDBPoolDef actionSelect dbPool
  let orgId = fromJust $ Database.MongoDB.lookup "organizationId" (fromJust organization) :: Maybe String
  let name = fromJust $ Database.MongoDB.lookup "name" (fromJust organization) :: Maybe String
  -- Add to config
  let actionModify =
        modify
          (select [] "appConfigs")
          [ "$set" =:
            ["organization" =: ["organizationId" =: orgId, "name" =: name, "affiliations" =: ([] :: [String])]]
          ]
  runMongoDBPoolDef actionModify dbPool
  -- Remove organization
  let actionDrop = dropCollection "organizations"
  runMongoDBPoolDef actionDrop dbPool
