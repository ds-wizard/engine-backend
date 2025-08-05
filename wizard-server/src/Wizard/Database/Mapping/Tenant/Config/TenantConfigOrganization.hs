module Wizard.Database.Mapping.Tenant.Config.TenantConfigOrganization where

import Database.PostgreSQL.Simple

import Shared.Common.Database.Mapping.Common ()
import Wizard.Model.Tenant.Config.TenantConfig

instance FromRow TenantConfigOrganization

instance ToRow TenantConfigOrganization
