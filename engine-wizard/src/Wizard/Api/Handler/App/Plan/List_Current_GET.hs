module Wizard.Api.Handler.App.Plan.List_Current_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Plan.AppPlanJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Plan.AppPlan
import Wizard.Service.Plan.AppPlanService

type List_Current_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "apps"
     :> "current"
     :> "plans"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [AppPlan])

list_current_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [AppPlan])
list_current_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getPlansForCurrentApp
