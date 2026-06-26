module Wizard.Database.Migration.Development.Tenant.Data.Tenants where

import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Constant.Tenant
import Wizard.Api.Resource.Tenant.TenantCreateDTO
import Wizard.Model.Tenant.Tenant
import WizardLib.Public.Model.Tenant.Module.TenantModule
import WizardLib.Public.Model.User.RolePermission

defaultTenant :: Tenant
defaultTenant =
  Tenant
    { uuid = defaultTenantUuid
    , tenantId = "default"
    , name = "Default Tenant"
    , serverDomain = "localhost:3000"
    , serverUrl = "http://localhost:3000/wizard-api"
    , clientUrl = "http://localhost:8080/wizard"
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

defaultTenantModules :: [TenantModule]
defaultTenantModules =
  [ defaultTenantModule {position = 0, moduleKey = "wizard", title = "Wizard", url = "http://localhost:8080/wizard"}
  , defaultTenantModule {position = 1, moduleKey = "admin", title = "Administration", url = "http://localhost:8080/admin"}
  , defaultTenantModule
      { position = 2
      , moduleKey = "integrationHub"
      , title = "Integration Hub"
      , url = "http://localhost:8080/integration-hub"
      , requiredPermission = Just _KNOWLEDGE_MODEL_EDITORS_USE_ROLE_PERMISSION
      }
  , defaultTenantModule
      { position = 3
      , moduleKey = "analytics"
      , title = "Analytics"
      , url = "http://localhost:8080/analytics"
      , requiredPermission = Just _SETTINGS_MANAGE_ROLE_PERMISSION
      }
  ]

defaultTenantModule :: TenantModule
defaultTenantModule =
  TenantModule
    { tenantUuid = defaultTenantUuid
    , position = 0
    , moduleKey = "wizard"
    , title = ""
    , description = ""
    , icon = ""
    , url = ""
    , external = False
    , requiredPermission = Nothing
    , enabled = True
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
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
