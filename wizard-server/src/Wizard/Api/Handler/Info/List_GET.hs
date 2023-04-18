module Wizard.Api.Handler.Info.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Api.Resource.Info.InfoDTO
import Shared.Common.Api.Resource.Info.InfoJM ()
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Service.Info.InfoService
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext

type List_GET =
  Header "Host" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] InfoDTO)

list_GET :: Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] InfoDTO)
list_GET mServerUrl =
  runInUnauthService mServerUrl NoTransaction $
    addTraceUuidHeader =<< getInfo
