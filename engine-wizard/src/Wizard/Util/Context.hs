module Wizard.Util.Context where

import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (liftIO, runReaderT)
import Data.Time

import LensesConfig
import Shared.Util.Uuid
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Util.Logger

runAppContextWithBaseContext :: AppContextM a -> BaseContext -> IO (Either String a)
runAppContextWithBaseContext function baseContext =
  appContextFromBaseContext Nothing baseContext >>= runAppContextWithAppContext function

runAppContextWithAppContext :: AppContextM a -> AppContext -> IO (Either String a)
runAppContextWithAppContext function appContext = do
  eResult <- liftIO $ runMonads (runAppContextM function) appContext
  case eResult of
    Right result -> return . Right $ result
    Left error ->
      runLogging' appContext $ do
        logError _CMP_SERVER ("Catched error: " ++ show error)
        return . Left $ show error

runMonads fn context = runExceptT $ runLogging' context $ runReaderT fn context

runLogging' context =
  let loggingLevel = context ^. serverConfig . logging . level
   in runLogging loggingLevel

appContextFromBaseContext :: Maybe UserDTO -> BaseContext -> IO AppContext
appContextFromBaseContext mUser baseContext = do
  cTraceUuid <- generateUuid
  now <- liftIO getCurrentTime
  return $
    AppContext
      { _appContextServerConfig = baseContext ^. serverConfig
      , _appContextLocalization = baseContext ^. localization
      , _appContextBuildInfoConfig = baseContext ^. buildInfoConfig
      , _appContextPool = baseContext ^. pool
      , _appContextMsgChannel = baseContext ^. msgChannel
      , _appContextHttpClientManager = baseContext ^. httpClientManager
      , _appContextRegistryClient = baseContext ^. registryClient
      , _appContextTraceUuid = cTraceUuid
      , _appContextCurrentUser = mUser
      , _appContextShutdownFlag = baseContext ^. shutdownFlag
      , _appContextCache = baseContext ^. cache
      }

baseContextFromAppContext :: AppContext -> BaseContext
baseContextFromAppContext appContext =
  BaseContext
    { _baseContextServerConfig = appContext ^. serverConfig
    , _baseContextLocalization = appContext ^. localization
    , _baseContextBuildInfoConfig = appContext ^. buildInfoConfig
    , _baseContextPool = appContext ^. pool
    , _baseContextMsgChannel = appContext ^. msgChannel
    , _baseContextHttpClientManager = appContext ^. httpClientManager
    , _baseContextRegistryClient = appContext ^. registryClient
    , _baseContextShutdownFlag = appContext ^. shutdownFlag
    , _baseContextCache = appContext ^. cache
    }
