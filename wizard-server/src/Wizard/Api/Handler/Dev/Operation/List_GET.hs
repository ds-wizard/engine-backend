module Wizard.Api.Handler.Dev.Operation.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Api.Resource.Dev.DevSectionDTO
import Shared.Common.Api.Resource.Dev.DevSectionJM ()
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Service.Dev.DevOperationService
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Dev.DevOperationDefinitions

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "dev-operations"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] [DevSectionDTO])

list_GET :: Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] [DevSectionDTO])
list_GET mTokenHeader mServerUrl =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getDevOperations sections
