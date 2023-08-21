module Registry.Api.Handler.Config.List_Bootstrap_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Api.Resource.Config.ClientConfigDTO
import Registry.Api.Resource.Config.ClientConfigJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.Config.Client.ClientConfigService
import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState

type List_Bootstrap_GET =
  "configs"
    :> "bootstrap"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ClientConfigDTO)

list_bootstrap_GET :: BaseContextM (Headers '[Header "x-trace-uuid" String] ClientConfigDTO)
list_bootstrap_GET =
  runInUnauthService NoTransaction $ addTraceUuidHeader =<< getClientConfig
