module Wizard.Api.Handler.Tenant.Usage.Api where

import Servant
import Servant.Swagger.Tags

import Wizard.Api.Handler.Tenant.Usage.Current_Wizard_GET
import Wizard.Api.Handler.Tenant.Usage.Detail_Wizard_GET
import Wizard.Model.Context.BaseContext

type TenantUsageAPI =
  Tags "Tenant Usage"
    :> ( Current_Wizard_GET
          :<|> Detail_Wizard_GET
       )

tenantUsageApi :: Proxy TenantUsageAPI
tenantUsageApi = Proxy

tenantUsageServer :: ServerT TenantUsageAPI BaseContextM
tenantUsageServer =
  current_wizard_GET
    :<|> detail_wizard_GET
