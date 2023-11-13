module Wizard.Api.Handler.Tenant.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Tenant.TenantChangeDTO
import Wizard.Api.Resource.Tenant.TenantChangeJM ()
import Wizard.Api.Resource.Tenant.TenantJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Tenant.TenantService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TenantChangeDTO
    :> "tenants"
    :> Capture "aUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] Tenant)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> TenantChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] Tenant)
detail_PUT mTokenHeader mServerUrl reqDto aUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyTenant aUuid reqDto
