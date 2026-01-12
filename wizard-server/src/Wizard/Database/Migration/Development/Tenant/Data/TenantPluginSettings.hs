module Wizard.Database.Migration.Development.Tenant.Data.TenantPluginSettings where

import Shared.Common.Util.Date
import Wizard.Database.Migration.Development.Plugin.Data.PluginSettings
import Wizard.Database.Migration.Development.Plugin.Data.Plugins
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Plugin.Plugin
import Wizard.Model.Tenant.PluginSettings.TenantPluginSettings
import Wizard.Model.Tenant.Tenant

defaultTenantPluginSettings :: TenantPluginSettings
defaultTenantPluginSettings =
  TenantPluginSettings
    { tenantUuid = defaultTenant.uuid
    , pluginUuid = plugin1.uuid
    , values = plugin1Values1
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

defaultTenantPluginSettingsEdited :: TenantPluginSettings
defaultTenantPluginSettingsEdited =
  defaultTenantPluginSettings
    { tenantUuid = defaultTenant.uuid
    , pluginUuid = plugin1.uuid
    , values = plugin1Values1Edited
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }

differentTenantPluginSettings :: TenantPluginSettings
differentTenantPluginSettings =
  TenantPluginSettings
    { tenantUuid = differentTenant.uuid
    , pluginUuid = plugin1.uuid
    , values = plugin1Values2
    , createdAt = dt' 2018 1 21
    , updatedAt = dt' 2018 1 21
    }
