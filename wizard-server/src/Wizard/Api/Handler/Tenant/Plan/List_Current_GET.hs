module Wizard.Api.Handler.Tenant.Plan.List_Current_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Tenant.Plan.TenantPlanJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Tenant.Plan.TenantPlan
import Wizard.Service.Tenant.Plan.PlanService

type List_Current_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "tenants"
    :> "current"
    :> "plans"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [TenantPlan])

list_current_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] [TenantPlan])
list_current_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getPlansForCurrentTenant
