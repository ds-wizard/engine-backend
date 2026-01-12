module Wizard.Service.User.PluginSettings.UserPluginSettingsMapper where

import qualified Data.Aeson as A
import Data.Time
import qualified Data.UUID as U

import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.User.UserPluginSettings

fromCreate :: A.Value -> UserDTO -> U.UUID -> U.UUID -> UTCTime -> UserPluginSettings
fromCreate reqDto user pluginUuid tenantUuid now = do
  UserPluginSettings
    { userUuid = user.uuid
    , pluginUuid = pluginUuid
    , values = reqDto
    , tenantUuid = tenantUuid
    , createdAt = now
    , updatedAt = now
    }

fromChange :: UserPluginSettings -> A.Value -> UTCTime -> UserPluginSettings
fromChange pluginSettings values now = do
  UserPluginSettings
    { userUuid = pluginSettings.userUuid
    , pluginUuid = pluginSettings.pluginUuid
    , values = values
    , tenantUuid = pluginSettings.tenantUuid
    , createdAt = pluginSettings.createdAt
    , updatedAt = now
    }
