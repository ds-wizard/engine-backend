module Wizard.Api.Handler.Tenant.PluginSettings.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Tenant.PluginSettings.Detail_GET
import Wizard.Api.Handler.Tenant.PluginSettings.Detail_PUT
import Wizard.Api.Handler.Tenant.PluginSettings.List_PUT
import Wizard.Model.Context.BaseContext

type TenantPluginSettingsAPI =
  Tags "Tenant Plugin Settings"
    :> ( List_PUT
          :<|> Detail_GET
          :<|> Detail_PUT
       )

tenantPluginSettingsApi :: Proxy TenantPluginSettingsAPI
tenantPluginSettingsApi = Proxy

tenantPluginSettingsServer :: ServerT TenantPluginSettingsAPI BaseContextM
tenantPluginSettingsServer =
  list_PUT
    :<|> detail_GET
    :<|> detail_PUT
