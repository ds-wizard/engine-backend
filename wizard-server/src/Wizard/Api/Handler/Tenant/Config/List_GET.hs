module Wizard.Api.Handler.Tenant.Config.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Tenant.Config.TenantConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Tenant.Config.ConfigService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> "current"
    :> "config"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TenantConfig)

list_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] TenantConfig)
list_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getCurrentTenantConfigDto
