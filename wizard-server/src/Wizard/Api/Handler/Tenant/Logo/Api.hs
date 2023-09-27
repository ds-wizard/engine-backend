module Wizard.Api.Handler.Tenant.Logo.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Tenant.Logo.List_DELETE
import Wizard.Api.Handler.Tenant.Logo.List_PUT
import Wizard.Model.Context.BaseContext

type TenantLogoAPI =
  Tags "Tenant Logo"
    :> ( List_PUT
          :<|> List_DELETE
       )

tenantLogoApi :: Proxy TenantLogoAPI
tenantLogoApi = Proxy

tenantLogoServer :: ServerT TenantLogoAPI BaseContextM
tenantLogoServer = list_PUT :<|> list_DELETE
