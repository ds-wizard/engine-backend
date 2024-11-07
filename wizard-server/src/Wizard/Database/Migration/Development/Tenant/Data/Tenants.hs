module Wizard.Database.Migration.Development.Tenant.Data.Tenants where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Constant.Tenant
import Wizard.Api.Resource.Tenant.TenantCreateDTO
import Wizard.Model.Tenant.Tenant

defaultTenant :: Tenant
defaultTenant =
  Tenant
    { uuid = defaultTenantUuid
    , tenantId = "default"
    , name = "Default Tenant"
    , serverDomain = "localhost:3000"
    , serverUrl = "http://localhost:3000/wizard-api"
    , clientUrl = "http://localhost:8080/wizard"
    , adminServerUrl = Nothing
    , adminClientUrl = Nothing
    , integrationHubServerUrl = Nothing
    , integrationHubClientUrl = Nothing
    , analyticsServerUrl = Nothing
    , analyticsClientUrl = Nothing
    , signalBridgeUrl = Nothing
    , enabled = True
    , state = ReadyForUseTenantState
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentTenant :: Tenant
differentTenant =
  Tenant
    { uuid = differentTenantUuid
    , tenantId = "different"
    , name = "Different Tenant"
    , serverDomain = "different-server.example.com"
    , serverUrl = "https://different-server.example.com/wizard-api"
    , clientUrl = "https://different-client.example.com/wizard"
    , adminServerUrl = Nothing
    , adminClientUrl = Nothing
    , integrationHubServerUrl = Nothing
    , integrationHubClientUrl = Nothing
    , analyticsServerUrl = Nothing
    , analyticsClientUrl = Nothing
    , signalBridgeUrl = Nothing
    , enabled = True
    , state = ReadyForUseTenantState
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentTenantEdited :: Tenant
differentTenantEdited =
  differentTenant
    { tenantId = "different-edited"
    , name = "EDtIED:Different Tenant"
    , serverDomain = "different-edited."
    , serverUrl = "https://different-edited./wizard-api"
    , clientUrl = "https://different-edited./wizard"
    }

tenantCreateDto :: TenantCreateDTO
tenantCreateDto =
  TenantCreateDTO
    { tenantId = "new-tenant-id"
    , tenantName = "New Tenant"
    , firstName = "Max"
    , lastName = "Planck"
    , email = "max.planck@example.com"
    , password = "password"
    }
