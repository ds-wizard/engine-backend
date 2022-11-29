module Wizard.Api.Handler.Info.List_GET where

import Control.Monad.Reader (ask)
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Resource.Info.InfoDTO
import Shared.Api.Resource.Info.InfoJM ()
import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext

type List_GET =
  Header "Host" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] InfoDTO)

list_GET :: Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] InfoDTO)
list_GET mServerUrl =
  runInUnauthService mServerUrl NoTransaction $
    addTraceUuidHeader =<< do
      appContext <- ask
      return
        InfoDTO
          { name = appContext.buildInfoConfig.name
          , version = appContext.buildInfoConfig.version
          , builtAt = appContext.buildInfoConfig.builtAt
          }
