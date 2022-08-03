module Wizard.Api.Handler.Config.List_Bootstrap_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Config.ClientConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.ClientConfigService

type List_Bootstrap_GET
   = Header "Host" String
     :> "configs"
     :> "bootstrap"
     :> QueryParam "clientUrl" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] ClientConfigDTO)

list_bootstrap_GET ::
     Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] ClientConfigDTO)
list_bootstrap_GET mServerUrl mClientUrl =
  runInUnauthService mServerUrl NoTransaction $ addTraceUuidHeader =<< getClientConfig mClientUrl
