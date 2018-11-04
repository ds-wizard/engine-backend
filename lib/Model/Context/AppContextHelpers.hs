module Model.Context.AppContextHelpers where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO, runReaderT)

import LensesConfig
import Model.Context.AppContext
import Util.Uuid

runAppContextWithBaseContext function baseContext = do
  traceUuid <- liftIO generateUuid
  let appContext =
        AppContext
        { _appContextConfig = baseContext ^. config
        , _appContextPool = baseContext ^. pool
        , _appContextMsgChannel = baseContext ^. msgChannel
        , _appContextTraceUuid = traceUuid
        , _appContextCurrentUser = Nothing
        }
  runReaderT (runAppContextM function) appContext
