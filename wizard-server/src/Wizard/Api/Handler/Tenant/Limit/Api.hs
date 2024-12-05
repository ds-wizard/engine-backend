module Wizard.Api.Handler.Tenant.Limit.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Tenant.Limit.List_PUT
import Wizard.Model.Context.BaseContext

type TenantLimitAPI =
  Tags "Tenant Limit"
    :> List_PUT

tenantLimitApi :: Proxy TenantLimitAPI
tenantLimitApi = Proxy

tenantLimitServer :: ServerT TenantLimitAPI BaseContextM
tenantLimitServer = list_PUT
