module Wizard.Util.Context where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (liftIO, runReaderT)
import Data.Pool
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Constant.Tenant
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserMapper

runAppContextWithBaseContext :: AppContextM a -> BaseContext -> IO (Either String a)
runAppContextWithBaseContext function baseContext =
  appContextFromBaseContext defaultTenantUuid (Just . toDTO $ userSystem) Transactional baseContext $
    runAppContextWithAppContext function

runAppContextWithBaseContext' :: AppContextM a -> BaseContext -> U.UUID -> IO (Either String a)
runAppContextWithBaseContext' function baseContext tenantUuid =
  appContextFromBaseContext tenantUuid (Just . toDTO $ userSystem) Transactional baseContext $
    runAppContextWithAppContext function

runAppContextWithBaseContext'' :: AppContextM a -> BaseContext -> IO (Either String a)
runAppContextWithBaseContext'' function baseContext =
  appContextFromBaseContext defaultTenantUuid (Just . toDTO $ userSystem) NoTransaction baseContext $
    runAppContextWithAppContext function

runAppContextWithAppContext :: AppContextM a -> AppContext -> IO (Either String a)
runAppContextWithAppContext function appContext = do
  eResult <- liftIO $ runMonads (runAppContextM function) appContext
  case eResult of
    Right result -> return . Right $ result
    Left error ->
      runLogging' appContext $ do
        logError _CMP_SERVER ("Catched error: " ++ show error)
        return . Left $ show error

runAppContextWithAppContext' :: AppContextM a -> AppContext -> IO (Either String a)
runAppContextWithAppContext' function appContext =
  withResource appContext.dbPool $ \dbConn -> do
    let updatedAppContext = appContext {dbConnection = Just dbConn}
    eResult <- liftIO $ runMonads (runAppContextM function) updatedAppContext
    case eResult of
      Right result -> return . Right $ result
      Left error ->
        runLogging' updatedAppContext $ do
          logError _CMP_SERVER ("Catched error: " ++ show error)
          return . Left $ show error

runMonads fn context = runExceptT $ runLogging' context $ runReaderT fn context

runLogging' context =
  let loggingLevel = context.serverConfig.logging.level
   in runLogging loggingLevel

appContextFromBaseContext tenantUuid mUser transactionState baseContext callback = do
  cTraceUuid <- generateUuid
  now <- liftIO getCurrentTime
  let appContext =
        AppContext
          { serverConfig = baseContext.serverConfig
          , buildInfoConfig = baseContext.buildInfoConfig
          , dbPool = baseContext.dbPool
          , dbConnection = Nothing
          , s3Client = baseContext.s3Client
          , httpClientManager = baseContext.httpClientManager
          , registryClient = baseContext.registryClient
          , traceUuid = cTraceUuid
          , currentTenantUuid = tenantUuid
          , currentUser = mUser
          , shutdownFlag = baseContext.shutdownFlag
          , cache = baseContext.cache
          }
  case transactionState of
    Transactional -> do
      withResource baseContext.dbPool $ \dbConn -> do
        let appContextWithConn = appContext {dbConnection = Just dbConn}
        callback appContextWithConn
    NoTransaction -> do
      callback appContext

baseContextFromAppContext :: AppContext -> BaseContext
baseContextFromAppContext appContext =
  BaseContext
    { serverConfig = appContext.serverConfig
    , buildInfoConfig = appContext.buildInfoConfig
    , dbPool = appContext.dbPool
    , s3Client = appContext.s3Client
    , httpClientManager = appContext.httpClientManager
    , registryClient = appContext.registryClient
    , shutdownFlag = appContext.shutdownFlag
    , cache = appContext.cache
    }
