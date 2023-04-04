module Registry.Api.Handler.Info.List_GET where

import Servant

import Registry.Api.Handler.Common
import Registry.Model.Context.BaseContext hiding (buildInfoConfig)
import Registry.Model.Context.ContextLenses ()
import Shared.Api.Handler.Common
import Shared.Api.Resource.Info.InfoDTO
import Shared.Api.Resource.Info.InfoJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Info.InfoService

type List_GET = Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] InfoDTO)

list_GET :: BaseContextM (Headers '[Header "x-trace-uuid" String] InfoDTO)
list_GET =
  runInUnauthService NoTransaction $
    addTraceUuidHeader =<< getInfo
