module Wizard.Database.Mapping.Tenant.TenantLimitBundle where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Tenant.Limit.TenantLimitBundle

instance FromRow TenantLimitBundle where
  fromRow = do
    uuid <- field
    users <- field
    activeUsers <- field
    knowledgeModels <- field
    knowledgeModelEditors <- field
    documentTemplates <- field
    questionnaires <- field
    documents <- field
    storage <- field
    createdAt <- field
    updatedAt <- field
    documentTemplateDrafts <- field
    locales <- field
    return $ TenantLimitBundle {..}

instance ToRow TenantLimitBundle where
  toRow TenantLimitBundle {..} =
    [ toField uuid
    , toField users
    , toField activeUsers
    , toField knowledgeModels
    , toField knowledgeModelEditors
    , toField documentTemplates
    , toField questionnaires
    , toField documents
    , toField storage
    , toField createdAt
    , toField updatedAt
    , toField documentTemplateDrafts
    , toField locales
    ]
