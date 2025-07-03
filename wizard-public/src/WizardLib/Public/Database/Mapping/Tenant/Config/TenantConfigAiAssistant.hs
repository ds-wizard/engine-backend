module WizardLib.Public.Database.Mapping.Tenant.Config.TenantConfigAiAssistant where

import Database.PostgreSQL.Simple

import WizardLib.Public.Model.Tenant.Config.TenantConfig

instance FromRow TenantConfigAiAssistant

instance ToRow TenantConfigAiAssistant
