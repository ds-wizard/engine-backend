module Wizard.Service.Tenant.PluginSettings.TenantPluginSettingsMapper where

import qualified Data.Aeson as A
import Data.Time
import qualified Data.UUID as U

import Wizard.Model.Tenant.PluginSettings.TenantPluginSettings

fromCreate :: A.Value -> U.UUID -> U.UUID -> UTCTime -> TenantPluginSettings
fromCreate reqDto pluginUuid tenantUuid now = do
  TenantPluginSettings
    { tenantUuid = tenantUuid
    , pluginUuid = pluginUuid
    , values = reqDto
    , createdAt = now
    , updatedAt = now
    }

fromChange :: TenantPluginSettings -> A.Value -> UTCTime -> TenantPluginSettings
fromChange pluginSettings values now = do
  TenantPluginSettings
    { tenantUuid = pluginSettings.tenantUuid
    , pluginUuid = pluginSettings.pluginUuid
    , values = values
    , createdAt = pluginSettings.createdAt
    , updatedAt = now
    }
