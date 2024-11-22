module Wizard.Api.Handler.Config.List_Bootstrap_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Config.ClientConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.Client.ClientConfigService

type List_Bootstrap_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "configs"
    :> "bootstrap"
    :> QueryParam "clientUrl" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ClientConfigDTO)

list_bootstrap_GET :: Maybe String -> Maybe String -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] ClientConfigDTO)
list_bootstrap_GET mTokenHeader mServerUrl mClientUrl =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< getClientConfig mServerUrl mClientUrl
