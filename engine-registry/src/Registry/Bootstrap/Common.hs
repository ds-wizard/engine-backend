module Registry.Bootstrap.Common where

import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (liftIO, runReaderT)

import LensesConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Util.Logger
import Shared.Util.Uuid

runAppContextWithBaseContext :: AppContextM a -> BaseContext -> IO ()
runAppContextWithBaseContext function baseContext = do
  traceUuid <- liftIO generateUuid
  let appContext =
        AppContext
          { _appContextServerConfig = baseContext ^. serverConfig
          , _appContextLocalization = baseContext ^. localization
          , _appContextBuildInfoConfig = baseContext ^. buildInfoConfig
          , _appContextPool = baseContext ^. pool
          , _appContextTraceUuid = traceUuid
          , _appContextCurrentOrganization = Nothing
          }
  let loggingLevel = baseContext ^. serverConfig . logging . level
  _ <- liftIO . runExceptT $ runLogging loggingLevel $ runReaderT (runAppContextM function) appContext
  return ()
