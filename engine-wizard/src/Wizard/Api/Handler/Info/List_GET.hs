module Wizard.Api.Handler.Info.List_GET where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Servant

import LensesConfig
import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Info.InfoDTO
import Wizard.Api.Resource.Info.InfoJM ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext

type List_GET = Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] InfoDTO)

list_GET :: BaseContextM (Headers '[ Header "x-trace-uuid" String] InfoDTO)
list_GET =
  runInUnauthService $
  addTraceUuidHeader =<< do
    buildInfoConfig <- asks _appContextBuildInfoConfig
    return
      InfoDTO
        { _infoDTOName = buildInfoConfig ^. name
        , _infoDTOVersion = buildInfoConfig ^. version
        , _infoDTOBuiltAt = buildInfoConfig ^. builtAt
        }
