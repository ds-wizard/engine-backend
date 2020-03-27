module Wizard.Api.Handler.Config.List_Bootstrap_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Config.ClientConfigDTO
import Wizard.Api.Resource.Config.ClientConfigJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.ClientConfigService

type List_Bootstrap_GET
   = "configs"
     :> "bootstrap"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] ClientConfigDTO)

list_bootstrap_GET :: BaseContextM (Headers '[ Header "x-trace-uuid" String] ClientConfigDTO)
list_bootstrap_GET = runInUnauthService $ addTraceUuidHeader =<< getClientConfig
