module WizardLib.Public.Database.Mapping.Tenant.Config.TenantConfigMail where

import Database.PostgreSQL.Simple

import Shared.Common.Database.Mapping.Common ()
import WizardLib.Public.Model.Tenant.Config.TenantConfig

instance FromRow TenantConfigMail

instance ToRow TenantConfigMail
