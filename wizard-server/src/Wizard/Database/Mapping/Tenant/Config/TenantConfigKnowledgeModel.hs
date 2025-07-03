module Wizard.Database.Mapping.Tenant.Config.TenantConfigKnowledgeModel where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Wizard.Model.Tenant.Config.TenantConfig

instance ToRow TenantConfigKnowledgeModel where
  toRow TenantConfigKnowledgeModel {..} =
    [ toField tenantUuid
    , toField public.enabled
    , toField integrationConfig
    , toField createdAt
    , toField updatedAt
    ]

instance FromRow TenantConfigKnowledgeModel where
  fromRow = do
    tenantUuid <- field
    publicEnabled <- field
    let public = TenantConfigKnowledgeModelPublic {enabled = publicEnabled, packages = []}
    integrationConfig <- field
    createdAt <- field
    updatedAt <- field
    return $ TenantConfigKnowledgeModel {..}

instance ToRow TenantConfigKnowledgeModelPublicPackagePattern

instance FromRow TenantConfigKnowledgeModelPublicPackagePattern
