module WizardLib.Public.Database.Mapping.Tenant.Config.TenantConfigFeatures where

import Database.PostgreSQL.Simple

import WizardLib.Public.Model.Tenant.Config.TenantConfig

instance FromRow TenantConfigFeatures

instance ToRow TenantConfigFeatures
