module Wizard.Bootstrap.Common where

import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (liftIO, runReaderT)

import LensesConfig
import Shared.Util.Uuid
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Util.Logger

runAppContextWithBaseContext :: AppContextM a -> BaseContext -> IO ()
runAppContextWithBaseContext function baseContext = do
  traceUuid <- liftIO generateUuid
  let appContext =
        AppContext
          { _appContextServerConfig = baseContext ^. serverConfig
          , _appContextLocalization = baseContext ^. localization
          , _appContextBuildInfoConfig = baseContext ^. buildInfoConfig
          , _appContextPool = baseContext ^. pool
          , _appContextMsgChannel = baseContext ^. msgChannel
          , _appContextHttpClientManager = baseContext ^. httpClientManager
          , _appContextRegistryClient = baseContext ^. registryClient
          , _appContextTraceUuid = traceUuid
          , _appContextCurrentUser = Nothing
          , _appContextCache = baseContext ^. cache
          }
  let loggingLevel = baseContext ^. serverConfig . logging . level
  _ <- liftIO . runExceptT $ runLogging loggingLevel $ runReaderT (runAppContextM function) appContext
  return ()
