module Wizard.Api.Handler.Tenant.Config.List_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Api.Resource.Tenant.Config.TenantConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Tenant.Config.ConfigService

type List_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TenantConfigChangeDTO
    :> "tenants"
    :> "current"
    :> "config"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TenantConfig)

list_PUT
  :: Maybe String
  -> Maybe String
  -> TenantConfigChangeDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] TenantConfig)
list_PUT mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyTenantConfigDto reqDto
