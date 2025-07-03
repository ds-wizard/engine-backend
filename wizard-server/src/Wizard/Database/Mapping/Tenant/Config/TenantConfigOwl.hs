module Wizard.Database.Mapping.Tenant.Config.TenantConfigOwl where

import Database.PostgreSQL.Simple

import Shared.Common.Database.Mapping.Common ()
import Wizard.Model.Tenant.Config.TenantConfig

instance FromRow TenantConfigOwl

instance ToRow TenantConfigOwl
