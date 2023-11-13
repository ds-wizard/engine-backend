module Wizard.Api.Handler.Tenant.Plan.List_Current_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Tenant.Plan.PlanService
import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanJM ()
import WizardLib.Public.Model.Tenant.Plan.TenantPlan

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
