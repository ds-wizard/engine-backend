module Wizard.Api.Handler.Usage.List_Current_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Usage.UsageDTO
import Wizard.Api.Resource.Usage.UsageJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Usage.UsageService

type List_Current_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "usage"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] UsageDTO)

list_current_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] UsageDTO)
list_current_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getUsageForCurrentApp
