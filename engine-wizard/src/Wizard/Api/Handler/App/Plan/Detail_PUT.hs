module Wizard.Api.Handler.App.Plan.Detail_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Plan.AppPlanChangeDTO
import Wizard.Api.Resource.Plan.AppPlanChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Plan.AppPlan
import Wizard.Service.Plan.AppPlanService

type Detail_PUT
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] AppPlanChangeDTO
     :> "apps"
     :> Capture "aUuid" String
     :> "plans"
     :> Capture "pUuid" String
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] AppPlan)

detail_PUT ::
     Maybe String
  -> Maybe String
  -> AppPlanChangeDTO
  -> String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] AppPlan)
detail_PUT mTokenHeader mServerUrl reqDto aUuid pUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< modifyPlan aUuid pUuid reqDto
