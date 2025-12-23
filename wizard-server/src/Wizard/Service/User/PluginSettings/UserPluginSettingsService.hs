module Wizard.Service.User.PluginSettings.UserPluginSettingsService where

import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as A
import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.User.UserPluginSettingsDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.UserPluginSettings
import Wizard.Service.User.PluginSettings.UserPluginSettingsMapper

getPluginSettings :: U.UUID -> AppContextM A.Value
getPluginSettings pluginUuid = do
  user <- getCurrentUser
  pluginSettings <- findUserPluginSettingsByUserUuidAndPluginUuid user.uuid pluginUuid
  return pluginSettings.values

createOrUpdatePluginSettings :: U.UUID -> A.Value -> AppContextM A.Value
createOrUpdatePluginSettings pluginUuid reqDto = do
  user <- getCurrentUser
  now <- liftIO getCurrentTime
  mPluginSettings <- findUserPluginSettingsByUserUuidAndPluginUuid' user.uuid pluginUuid
  case mPluginSettings of
    Just pluginSettings -> do
      let pluginSettingsUpdated = fromChange pluginSettings reqDto now
      updateUserPluginSettings pluginSettingsUpdated
      return pluginSettingsUpdated.values
    Nothing -> do
      tenantUuid <- asks currentTenantUuid
      let pluginSettings = fromCreate reqDto user pluginUuid tenantUuid now
      insertUserPluginSettings pluginSettings
      return pluginSettings.values
