module Wizard.Api.Handler.Level.List_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Level.LevelJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Level.Level
import Wizard.Service.Level.LevelService

type List_GET
   = Header "Authorization" String
     :> "levels"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [Level])

list_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [Level])
list_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService -> runInAuthService $ addTraceUuidHeader =<< getLevels
