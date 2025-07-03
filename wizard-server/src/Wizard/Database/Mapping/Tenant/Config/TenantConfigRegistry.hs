module Wizard.Database.Mapping.Tenant.Config.TenantConfigRegistry where

import Database.PostgreSQL.Simple

import Shared.Common.Database.Mapping.Common ()
import Wizard.Model.Tenant.Config.TenantConfig

instance FromRow TenantConfigRegistry

instance ToRow TenantConfigRegistry
