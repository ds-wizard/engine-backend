module WizardLib.Public.Database.Migration.Development.Tenant.Data.Tenants where

import Shared.Common.Constant.Tenant
import WizardLib.Public.Api.Resource.Tenant.TenantSuggestionDTO

tenantSuggestion :: TenantSuggestionDTO
tenantSuggestion =
  TenantSuggestionDTO
    { uuid = defaultTenantUuid
    , name = "Default Tenant"
    , logoUrl = Nothing
    , primaryColor = Nothing
    , clientUrl = "http://localhost:8080/wizard"
    }
