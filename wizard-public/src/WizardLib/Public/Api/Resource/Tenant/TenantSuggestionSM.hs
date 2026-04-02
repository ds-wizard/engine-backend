module WizardLib.Public.Api.Resource.Tenant.TenantSuggestionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.Tenant.TenantSuggestionJM ()
import WizardLib.Public.Database.Migration.Development.Tenant.Data.Tenants
import WizardLib.Public.Model.Tenant.TenantSuggestion

instance ToSchema TenantSuggestion where
  declareNamedSchema = toSwagger tenantSuggestion
