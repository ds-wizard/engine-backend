module Wizard.Api.Handler.Level.List_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Level.LevelJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Level.Level
import Wizard.Service.Level.LevelService

type List_GET
   = "levels"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [Level])

list_GET :: BaseContextM (Headers '[ Header "x-trace-uuid" String] [Level])
list_GET = runInUnauthService $ addTraceUuidHeader =<< getLevels
