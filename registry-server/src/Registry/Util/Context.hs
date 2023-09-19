module Registry.Util.Context where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (liftIO, runReaderT)
import Data.Pool
import Data.Time

import Registry.Model.Config.ServerConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.BaseContext
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Util.Logger
import Shared.Common.Util.Uuid

runAppContextWithBaseContext :: AppContextM a -> BaseContext -> IO (Either String a)
runAppContextWithBaseContext function baseContext =
  appContextFromBaseContext (Just orgGlobal) Transactional baseContext $
    runAppContextWithAppContext function

runAppContextWithBaseContext'' :: AppContextM a -> BaseContext -> IO (Either String a)
runAppContextWithBaseContext'' function baseContext =
  appContextFromBaseContext (Just orgGlobal) NoTransaction baseContext $
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

appContextFromBaseContext currentOrganization transactionState baseContext callback = do
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
          , traceUuid = cTraceUuid
          , currentOrganization = currentOrganization
          }
  case transactionState of
    Transactional -> do
      withResource baseContext.dbPool $ \dbConn -> do
        let appContextWithConn = appContext {dbConnection = Just dbConn}
        callback appContextWithConn
    NoTransaction -> do
      callback appContext
