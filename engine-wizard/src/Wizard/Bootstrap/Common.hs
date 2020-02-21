module Wizard.Bootstrap.Common where

import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (liftIO, runReaderT)

import LensesConfig
import Shared.Util.Uuid
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext

runAppContextWithBaseContext :: AppContextM a -> BaseContext -> IO ()
runAppContextWithBaseContext function baseContext = do
  traceUuid <- liftIO generateUuid
  let appContext =
        AppContext
          { _appContextApplicationConfig = baseContext ^. appConfig
          , _appContextLocalization = baseContext ^. localization
          , _appContextBuildInfoConfig = baseContext ^. buildInfoConfig
          , _appContextPool = baseContext ^. pool
          , _appContextMsgChannel = baseContext ^. msgChannel
          , _appContextHttpClientManager = baseContext ^. httpClientManager
          , _appContextTraceUuid = traceUuid
          , _appContextCurrentUser = Nothing
          }
  _ <- liftIO . runExceptT $ runStdoutLoggingT $ runReaderT (runAppContextM function) appContext
  return ()
