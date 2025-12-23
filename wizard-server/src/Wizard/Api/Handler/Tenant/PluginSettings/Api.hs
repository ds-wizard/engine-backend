module Wizard.Api.Handler.Tenant.PluginSettings.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Tenant.PluginSettings.Detail_GET
import Wizard.Api.Handler.Tenant.PluginSettings.Detail_PUT
import Wizard.Model.Context.BaseContext

type TenantPluginSettingsAPI =
  Tags "Tenant Plugin Settings"
    :> ( Detail_GET
          :<|> Detail_PUT
       )

tenantPluginSettingsApi :: Proxy TenantPluginSettingsAPI
tenantPluginSettingsApi = Proxy

tenantPluginSettingsServer :: ServerT TenantPluginSettingsAPI BaseContextM
tenantPluginSettingsServer =
  detail_GET
    :<|> detail_PUT
