module Registry.Bootstrap.Common where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (liftIO, runReaderT)
import Data.Pool

import Registry.Model.Config.ServerConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import Registry.Util.Logger
import Shared.Model.Config.ServerConfig
import Shared.Util.Uuid

runAppContextWithBaseContext :: AppContextM a -> BaseContext -> IO a
runAppContextWithBaseContext function baseContext = do
  traceUuid <- liftIO generateUuid
  withResource baseContext.dbPool $ \dbConnection -> do
    let appContext =
          AppContext
            { serverConfig = baseContext.serverConfig
            , buildInfoConfig = baseContext.buildInfoConfig
            , dbPool = baseContext.dbPool
            , dbConnection = Just dbConnection
            , s3Client = baseContext.s3Client
            , traceUuid = traceUuid
            , currentOrganization = Nothing
            }
    let loggingLevel = baseContext.serverConfig.logging.level
    (Right result) <- liftIO . runExceptT $ runLogging loggingLevel $ runReaderT (runAppContextM function) appContext
    return result
