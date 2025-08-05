module Wizard.Database.Mapping.Tenant.Config.TenantConfigPrivacyAndSupport where

import Database.PostgreSQL.Simple

import Wizard.Model.Tenant.Config.TenantConfig

instance ToRow TenantConfigPrivacyAndSupport

instance FromRow TenantConfigPrivacyAndSupport
