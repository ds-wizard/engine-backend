module WizardLib.Public.Api.Resource.Tenant.TenantSuggestionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.Tenant.TenantSuggestionDTO
import WizardLib.Public.Api.Resource.Tenant.TenantSuggestionJM ()
import WizardLib.Public.Database.Migration.Development.Tenant.Data.Tenants

instance ToSchema TenantSuggestionDTO where
  declareNamedSchema = toSwagger tenantSuggestion
