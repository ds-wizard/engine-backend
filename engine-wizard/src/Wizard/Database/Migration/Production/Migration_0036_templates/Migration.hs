module Wizard.Database.Migration.Production.Migration_0036_templates.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Time
import Database.MongoDB
import Database.MongoDB.Migration.Entity
import Database.Persist.MongoDB (ConnectionPool, runMongoDBPoolDef)

import Wizard.Database.Migration.Production.Migration_0036_templates.Data.Templates

definition = (meta, migrate)

meta =
  MigrationMeta {mmNumber = 36, mmName = "New Templates", mmDescription = "Enhance appConfigs and refactor templates"}

migrate :: ConnectionPool -> LoggingT IO (Maybe Error)
migrate dbPool = do
  migrateAppConfig dbPool
  migrateTemplate dbPool
  return Nothing

-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------
migrateAppConfig dbPool = do
  let actionModify =
        modify
          (select [] "appConfigs")
          [ "$set" =:
            [ "questionnaire.questionnaireVisibility.defaultValue" =: "PrivateQuestionnaire"
            , "questionnaire.summaryReport.enabled" =: True
            , "privacyAndSupport.termsOfServiceUrl" =: (Nothing :: Maybe String)
            ]
          ]
  runMongoDBPoolDef actionModify dbPool
  let actionRename = modify (select [] "appConfigs") ["$rename" =: ["knowledgeModelRegistry" =: "registry"]]
  runMongoDBPoolDef actionRename dbPool
  return Nothing

-- ------------------------------------------------------------------------------------------------
-- ------------------------------------------------------------------------------------------------
migrateTemplate dbPool = do
  addDefaultTemplate dbPool
  addPermissionToAdmins dbPool
  renameAppConfigsRecommendedTemplateUuid dbPool
  renameDocumentTemplateUuid dbPool
  renameTemplateReferences dbPool

addDefaultTemplate dbPool = do
  now <- liftIO getCurrentTime
  let action = insert "templates" (template now)
  runMongoDBPoolDef action dbPool

addPermissionToAdmins dbPool = do
  let action = modify (select ["role" =: "admin"] "users") ["$push" =: ["permissions" =: "TML_PERM"]]
  runMongoDBPoolDef action dbPool

renameAppConfigsRecommendedTemplateUuid dbPool = do
  let action =
        modify
          (select [] "appConfigs")
          ["$rename" =: ["template.recommendedTemplateUuid" =: "template.recommendedTemplateId"]]
  runMongoDBPoolDef action dbPool
  return Nothing

renameDocumentTemplateUuid dbPool = do
  let action = modify (select [] "documents") ["$rename" =: ["templateUuid" =: "templateId"]]
  runMongoDBPoolDef action dbPool

renameTemplateReferences dbPool = do
  let defUuid = "43a3fdd1-8535-42e0-81a7-5edbff296e65"
  let defId = "dsw:questionnaire-report:1.0.0"
  let scUuid = "43a3fdd1-8535-42e0-81a7-5edbff296e66"
  let scId = "dsw:science-europe:1.0.0"
  -- 1. Rename reference in appConfigs
  renameField dbPool "template.recommendedTemplateUuid" "appConfigs" defUuid defId
  renameField dbPool "template.recommendedTemplateUuid" "appConfigs" scUuid scId
  -- 2. Rename reference in questionnaires
  renameField dbPool "templateId" "questionnaires" defUuid defId
  renameField dbPool "templateId" "questionnaires" scUuid scId
  -- 3. Rename reference in documents
  renameField dbPool "templateId" "documents" defUuid defId
  renameField dbPool "templateId" "documents" scUuid scId

renameField dbPool fieldName collection oldValue newValue = do
  let action = modify (select [fieldName =: oldValue] collection) ["$set" =: [fieldName =: newValue]]
  runMongoDBPoolDef action dbPool
  return Nothing
