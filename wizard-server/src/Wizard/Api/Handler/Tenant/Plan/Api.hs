module Wizard.Api.Handler.Tenant.Plan.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Tenant.Plan.Detail_DELETE
import Wizard.Api.Handler.Tenant.Plan.Detail_PUT
import Wizard.Api.Handler.Tenant.Plan.List_Current_GET
import Wizard.Api.Handler.Tenant.Plan.List_POST
import Wizard.Model.Context.BaseContext

type TenantPlanAPI =
  Tags "Tenant Plan"
    :> ( List_Current_GET
          :<|> List_POST
          :<|> Detail_PUT
          :<|> Detail_DELETE
       )

tenantPlanApi :: Proxy TenantPlanAPI
tenantPlanApi = Proxy

tenantPlanServer :: ServerT TenantPlanAPI BaseContextM
tenantPlanServer = list_current_GET :<|> list_POST :<|> detail_PUT :<|> detail_DELETE
