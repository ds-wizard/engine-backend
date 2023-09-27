module Wizard.Database.Mapping.Tenant.TenantPlan where

import Database.PostgreSQL.Simple

import Wizard.Model.Tenant.Plan.TenantPlan

instance ToRow TenantPlan

instance FromRow TenantPlan
