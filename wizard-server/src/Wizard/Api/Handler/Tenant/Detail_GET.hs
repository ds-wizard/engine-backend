module Wizard.Api.Handler.Tenant.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Tenant.TenantDetailDTO
import Wizard.Api.Resource.Tenant.TenantDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Tenant.TenantService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> Capture "aUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TenantDetailDTO)

detail_GET
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] TenantDetailDTO)
detail_GET mTokenHeader mServerUrl aUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getTenantByUuid aUuid
