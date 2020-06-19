module Wizard.Database.Migration.Production.Migration_0038_template_management.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

import Wizard.Database.Migration.Production.Migration_0038_template_management.Data.Templates

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 38, mmName = "Template management", mmDescription = "Move template to the database"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addDefaultTemplate dbPool
  addPermissionToAdmins dbPool
  renameRecommendedTemplateUuid dbPool
  return Nothing

addDefaultTemplate dbPool = do
  template <- liftIO getDefaultTemplate
  let action = insert "templates" template
  runMongoDBPoolDef action dbPool

addPermissionToAdmins dbPool = do
  let action = modify (select ["role" =: "admin"] "users") ["$push" =: ["permissions" =: "TML_PERM"]]
  runMongoDBPoolDef action dbPool

renameRecommendedTemplateUuid dbPool = do
  let action =
        modify
          (select [] "appConfigs")
          ["$rename" =: ["template.recommendedTemplateUuid" =: "template.recommendedTemplateId"]]
  runMongoDBPoolDef action dbPool
  return Nothing
