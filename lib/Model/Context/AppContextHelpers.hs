module Model.Context.AppContextHelpers where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO, runReaderT)

import LensesConfig
import Model.Context.AppContext
import Util.Uuid

runAppContextWithBaseContext function baseContext = do
  traceUuid <- liftIO generateUuid
  let dswConfig = baseContext ^. config
  let dbPool = baseContext ^. pool
  let appContext =
        AppContext
        { _appContextConfig = dswConfig
        , _appContextPool = dbPool
        , _appContextTraceUuid = traceUuid
        , _appContextCurrentUser = Nothing
        }
  runReaderT (runAppContextM function) appContext
