module Wizard.Api.Handler.App.Plan.Detail_DELETE where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Plan.AppPlanService

type Detail_DELETE
   = Header "Authorization" String
     :> Header "Host" String
     :> "apps"
     :> Capture "aUuid" String
     :> "plans"
     :> Capture "pUuid" String
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

detail_DELETE ::
     Maybe String
  -> Maybe String
  -> String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader mServerUrl aUuid pUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      deletePlan aUuid pUuid
      return NoContent
