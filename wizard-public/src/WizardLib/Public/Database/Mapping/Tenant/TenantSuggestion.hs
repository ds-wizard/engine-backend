module WizardLib.Public.Database.Mapping.Tenant.TenantSuggestion where

import Database.PostgreSQL.Simple

import WizardLib.Public.Model.Tenant.TenantSuggestion

instance FromRow TenantSuggestion
