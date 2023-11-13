module WizardLib.Public.Database.Mapping.Tenant.TenantPlan where

import Database.PostgreSQL.Simple

import WizardLib.Public.Model.Tenant.Plan.TenantPlan

instance ToRow TenantPlan

instance FromRow TenantPlan
