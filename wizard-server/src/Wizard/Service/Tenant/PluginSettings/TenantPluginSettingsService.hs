module Wizard.Service.Tenant.PluginSettings.TenantPluginSettingsService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as A
import Data.Time
import qualified Data.UUID as U

import Wizard.Database.DAO.Tenant.PluginSettings.TenantPluginSettingsDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.PluginSettings.TenantPluginSettings
import Wizard.Service.Tenant.PluginSettings.TenantPluginSettingsMapper

getPluginSettings :: U.UUID -> AppContextM A.Value
getPluginSettings pluginUuid = do
  pluginSettings <- findTenantPluginSettingsByPluginUuid pluginUuid
  return pluginSettings.values

createOrUpdatePluginSettings :: U.UUID -> A.Value -> AppContextM A.Value
createOrUpdatePluginSettings pluginUuid reqDto = do
  now <- liftIO getCurrentTime
  mPluginSettings <- findTenantPluginSettingsByPluginUuid' pluginUuid
  case mPluginSettings of
    Just pluginSettings -> do
      let pluginSettingsUpdated = fromChange pluginSettings reqDto now
      updateTenantPluginSettings pluginSettingsUpdated
      return pluginSettingsUpdated.values
    Nothing -> do
      tenantUuid <- asks currentTenantUuid
      let pluginSettings = fromCreate reqDto pluginUuid tenantUuid now
      insertTenantPluginSettings pluginSettings
      return pluginSettings.values
