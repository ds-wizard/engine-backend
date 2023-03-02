module Wizard.Api.Handler.Info.List_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Api.Resource.Info.InfoDTO
import Shared.Api.Resource.Info.InfoJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Info.InfoService
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext

type List_GET =
  Header "Host" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] InfoDTO)

list_GET :: Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] InfoDTO)
list_GET mServerUrl =
  runInUnauthService mServerUrl NoTransaction $
    addTraceUuidHeader =<< getInfo
