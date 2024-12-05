module Wizard.Api.Handler.Tenant.Limit.List_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Tenant.Limit.LimitService
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageDTO
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageJM ()
import WizardLib.Public.Model.Tenant.Limit.TenantLimitBundleChange

type List_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TenantLimitBundleChange
    :> "tenants"
    :> Capture "uuid" U.UUID
    :> "limits"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] WizardUsageDTO)

list_PUT
  :: Maybe String
  -> Maybe String
  -> TenantLimitBundleChange
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] WizardUsageDTO)
list_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyLimitBundle uuid reqDto
