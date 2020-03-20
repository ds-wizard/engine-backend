module Registry.Api.Handler.Info.List_GET where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Servant

import LensesConfig
import Registry.Api.Handler.Common
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Shared.Api.Handler.Common
import Shared.Api.Resource.Info.InfoDTO
import Shared.Api.Resource.Info.InfoJM ()

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
