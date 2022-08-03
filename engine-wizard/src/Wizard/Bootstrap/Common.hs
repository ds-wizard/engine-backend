module Wizard.Bootstrap.Common where

import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (liftIO, runReaderT)
import Data.Pool

import LensesConfig
import Shared.Constant.App
import Shared.Util.Uuid
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserMapper
import Wizard.Util.Logger

runAppContextWithBaseContext :: AppContextM a -> BaseContext -> IO a
runAppContextWithBaseContext function baseContext = do
  traceUuid <- liftIO generateUuid
  withResource (baseContext ^. dbPool) $ \dbConnection -> do
    let appContext =
          AppContext
            { _appContextServerConfig = baseContext ^. serverConfig
            , _appContextLocalization = baseContext ^. localization
            , _appContextBuildInfoConfig = baseContext ^. buildInfoConfig
            , _appContextDbPool = baseContext ^. dbPool
            , _appContextDbConnection = Just dbConnection
            , _appContextS3Client = baseContext ^. s3Client
            , _appContextHttpClientManager = baseContext ^. httpClientManager
            , _appContextRegistryClient = baseContext ^. registryClient
            , _appContextTraceUuid = traceUuid
            , _appContextAppUuid = defaultAppUuid
            , _appContextCurrentUser = Just . toDTO $ userSystem
            , _appContextShutdownFlag = baseContext ^. shutdownFlag
            , _appContextCache = baseContext ^. cache
            }
    let loggingLevel = baseContext ^. serverConfig . logging . level
    (Right result) <- liftIO . runExceptT $ runLogging loggingLevel $ runReaderT (runAppContextM function) appContext
    return result
