module Wizard.Api.Handler.Tenant.Config.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Tenant.Config.List_GET
import Wizard.Api.Handler.Tenant.Config.List_PUT
import Wizard.Model.Context.BaseContext

type TenantConfigAPI =
  Tags "Tenant Config"
    :> ( List_GET
          :<|> List_PUT
       )

tenantConfigApi :: Proxy TenantConfigAPI
tenantConfigApi = Proxy

tenantConfigServer :: ServerT TenantConfigAPI BaseContextM
tenantConfigServer = list_GET :<|> list_PUT
