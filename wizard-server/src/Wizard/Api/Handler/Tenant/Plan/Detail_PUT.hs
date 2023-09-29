module Wizard.Api.Handler.Tenant.Plan.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Tenant.Plan.PlanService
import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanChangeDTO
import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanChangeJM ()
import WizardLib.Public.Model.Tenant.Plan.TenantPlan

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TenantPlanChangeDTO
    :> "tenants"
    :> Capture "tenantUuid" U.UUID
    :> "plans"
    :> Capture "planUuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TenantPlan)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> TenantPlanChangeDTO
  -> U.UUID
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] TenantPlan)
detail_PUT mTokenHeader mServerUrl reqDto aUuid pUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyPlan aUuid pUuid reqDto
