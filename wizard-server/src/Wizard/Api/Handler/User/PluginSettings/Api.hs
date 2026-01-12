module Wizard.Api.Handler.User.PluginSettings.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.User.PluginSettings.Detail_GET
import Wizard.Api.Handler.User.PluginSettings.Detail_PUT
import Wizard.Model.Context.BaseContext

type PluginSettingsAPI =
  Tags "User Plugin Settings"
    :> ( Detail_GET
          :<|> Detail_PUT
       )

pluginSettingsApi :: Proxy PluginSettingsAPI
pluginSettingsApi = Proxy

pluginSettingsServer :: ServerT PluginSettingsAPI BaseContextM
pluginSettingsServer =
  detail_GET
    :<|> detail_PUT
