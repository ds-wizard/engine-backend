module Wizard.Api.Handler.Tenant.Plan.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Tenant.Plan.TenantPlanChangeDTO
import Wizard.Api.Resource.Tenant.Plan.TenantPlanChangeJM ()
import Wizard.Api.Resource.Tenant.Plan.TenantPlanJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Tenant.Plan.TenantPlan
import Wizard.Service.Tenant.Plan.PlanService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] TenantPlanChangeDTO
    :> "tenants"
    :> Capture "tenantUuid" U.UUID
    :> "plans"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] TenantPlan)

list_POST
  :: Maybe String
  -> Maybe String
  -> TenantPlanChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] TenantPlan)
list_POST mTokenHeader mServerUrl reqDto tenantUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createPlan tenantUuid reqDto
