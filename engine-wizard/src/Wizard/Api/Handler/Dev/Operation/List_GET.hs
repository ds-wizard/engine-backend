module Wizard.Api.Handler.Dev.Operation.List_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Dev.DevJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Dev.Dev
import Wizard.Service.Dev.DevOperationService

type List_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "dev-operations"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [DevSection])

list_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [DevSection])
list_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< getDevOperations
