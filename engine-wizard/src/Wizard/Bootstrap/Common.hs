module Wizard.Bootstrap.Common where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (liftIO, runReaderT)
import Data.Pool

import Shared.Constant.App
import Shared.Model.Config.ServerConfig
import Shared.Util.Uuid
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserMapper
import Wizard.Util.Logger

runAppContextWithBaseContext :: AppContextM a -> BaseContext -> IO a
runAppContextWithBaseContext function baseContext = do
  traceUuid <- liftIO generateUuid
  withResource baseContext.dbPool $ \dbConnection -> do
    let appContext =
          AppContext
            { serverConfig = baseContext.serverConfig
            , localization = baseContext.localization
            , buildInfoConfig = baseContext.buildInfoConfig
            , dbPool = baseContext.dbPool
            , dbConnection = Just dbConnection
            , s3Client = baseContext.s3Client
            , httpClientManager = baseContext.httpClientManager
            , registryClient = baseContext.registryClient
            , traceUuid = traceUuid
            , currentAppUuid = defaultAppUuid
            , currentUser = Just . toDTO $ userSystem
            , shutdownFlag = baseContext.shutdownFlag
            , cache = baseContext.cache
            }
    let loggingLevel = baseContext.serverConfig.logging.level
    (Right result) <- liftIO . runExceptT $ runLogging loggingLevel $ runReaderT (runAppContextM function) appContext
    return result
