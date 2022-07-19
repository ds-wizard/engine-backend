module Wizard.Util.Context where

import Control.Lens ((&), (?~), (^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (liftIO, runReaderT)
import Data.Pool
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Constant.App
import Shared.Model.Context.TransactionState
import Shared.Util.Uuid
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserMapper
import Wizard.Util.Logger

runAppContextWithBaseContext :: AppContextM a -> BaseContext -> IO (Either String a)
runAppContextWithBaseContext function baseContext =
  appContextFromBaseContext defaultAppUuid (Just . toDTO $ userSystem) Transactional baseContext $
  runAppContextWithAppContext function

runAppContextWithBaseContext' :: AppContextM a -> BaseContext -> U.UUID -> IO (Either String a)
runAppContextWithBaseContext' function baseContext appUuid =
  appContextFromBaseContext appUuid (Just . toDTO $ userSystem) Transactional baseContext $
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

runMonads fn context = runExceptT $ runLogging' context $ runReaderT fn context

runLogging' context =
  let loggingLevel = context ^. serverConfig . logging . level
   in runLogging loggingLevel

appContextFromBaseContext appUuid mUser transactionState baseContext callback = do
  cTraceUuid <- generateUuid
  now <- liftIO getCurrentTime
  let appContext =
        AppContext
          { _appContextServerConfig = baseContext ^. serverConfig
          , _appContextLocalization = baseContext ^. localization
          , _appContextBuildInfoConfig = baseContext ^. buildInfoConfig
          , _appContextDbPool = baseContext ^. dbPool
          , _appContextDbConnection = Nothing
          , _appContextS3Client = baseContext ^. s3Client
          , _appContextHttpClientManager = baseContext ^. httpClientManager
          , _appContextRegistryClient = baseContext ^. registryClient
          , _appContextTraceUuid = cTraceUuid
          , _appContextAppUuid = appUuid
          , _appContextCurrentUser = mUser
          , _appContextShutdownFlag = baseContext ^. shutdownFlag
          , _appContextCache = baseContext ^. cache
          }
  case transactionState of
    Transactional -> do
      withResource (baseContext ^. dbPool) $ \dbConn -> do
        let appContextWithConn = appContext & dbConnection ?~ dbConn
        callback appContextWithConn
    NoTransaction -> do
      callback appContext

baseContextFromAppContext :: AppContext -> BaseContext
baseContextFromAppContext appContext =
  BaseContext
    { _baseContextServerConfig = appContext ^. serverConfig
    , _baseContextLocalization = appContext ^. localization
    , _baseContextBuildInfoConfig = appContext ^. buildInfoConfig
    , _baseContextDbPool = appContext ^. dbPool
    , _baseContextS3Client = appContext ^. s3Client
    , _baseContextHttpClientManager = appContext ^. httpClientManager
    , _baseContextRegistryClient = appContext ^. registryClient
    , _baseContextShutdownFlag = appContext ^. shutdownFlag
    , _baseContextCache = appContext ^. cache
    }
