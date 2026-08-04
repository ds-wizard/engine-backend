module WizardLib.Public.Database.Mapping.Tenant.Module.TenantModule where

import Database.PostgreSQL.Simple

import Shared.Common.Database.Mapping.Common ()
import WizardLib.Public.Model.Tenant.Module.TenantModule

instance FromRow TenantModule

instance ToRow TenantModule
