module Wizard.Api.Handler.Tenant.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Tenant.Config.Api
import Wizard.Api.Handler.Tenant.Detail_DELETE
import Wizard.Api.Handler.Tenant.Detail_GET
import Wizard.Api.Handler.Tenant.Detail_PUT
import Wizard.Api.Handler.Tenant.Limit.Api
import Wizard.Api.Handler.Tenant.List_GET
import Wizard.Api.Handler.Tenant.List_POST
import Wizard.Api.Handler.Tenant.Usage.Api
import Wizard.Model.Context.BaseContext

type TenantAPI =
  Tags "Tenant"
    :> ( List_GET
          :<|> List_POST
          :<|> Detail_GET
          :<|> Detail_PUT
          :<|> Detail_DELETE
          :<|> TenantConfigAPI
          :<|> TenantLimitAPI
          :<|> TenantUsageAPI
       )

tenantApi :: Proxy TenantAPI
tenantApi = Proxy

tenantServer :: ServerT TenantAPI BaseContextM
tenantServer =
  list_GET
    :<|> list_POST
    :<|> detail_GET
    :<|> detail_PUT
    :<|> detail_DELETE
    :<|> tenantConfigServer
    :<|> tenantLimitServer
    :<|> tenantUsageServer
