module Wizard.Api.Handler.Tenant.Plan.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Tenant.Plan.PlanService
import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanChangeDTO
import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanChangeJM ()
import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanJM ()
import WizardLib.Public.Model.Tenant.Plan.TenantPlan

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
