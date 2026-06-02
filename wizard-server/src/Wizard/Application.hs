module Wizard.Application where

import Control.Concurrent
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)
import System.Environment (lookupEnv, setEnv)

import Shared.Common.Application
import Shared.Common.Bootstrap.AwsAppConfig
import Shared.Common.Bootstrap.Web
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig
import Shared.Worker.Bootstrap.Worker
import Wizard.Api.Middleware.LoggingMiddleware
import Wizard.Api.Sentry
import Wizard.Api.Web
import Wizard.Cache.CacheFactory
import Wizard.Constant.ASCIIArt
import Wizard.Constant.Resource
import qualified Wizard.Database.Migration.Development.Migration as DevDB
import qualified Wizard.Database.Migration.Production.Migration as ProdDB
import Wizard.Integration.Http.Common.ServantClient
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext
import Wizard.Model.Context.ContextMappers
import Wizard.Service.Config.Server.ServerConfigValidation
import Wizard.Worker.CronWorkers
import Wizard.Worker.PermanentWorkers
import WizardLib.Public.Util.Jinja (verifyJinja)

runApplication :: IO ()
runApplication =
  runWebServerWithWorkers
    [putStrLn asciiLogo, verifyJinja]
    serverConfigFile
    validateServerConfig
    buildInfoFile
    createBaseContext
    ProdDB.migrationDefinitions
    DevDB.runMigration
    afterDbMigrationHook
    runWebServer
    runWorker

createBaseContext :: (MonadIO m, MonadLogger m) => ServerConfig -> BuildInfoConfig -> Pool Connection -> MinioConn -> Manager -> MVar () -> m BaseContext
createBaseContext serverConfig buildInfoConfig dbPool s3Client httpClientManager shutdownFlag = do
  registryClient <- liftIO $ createRegistryClient serverConfig httpClientManager
  cache <- liftIO (createServerCache serverConfig)
  return BaseContext {..}

afterDbMigrationHook :: BaseContext -> IO ()
afterDbMigrationHook context = do
  mAwsAppConfig <- lookupEnv "AWS_APP_CONFIG"
  case mAwsAppConfig of
    Just _ -> do
      (path, poller) <- resolveConfigPath context.serverConfig.general.integrationConfig
      setEnv "INTEGRATION_CONFIG_PATH" path
      _ <- forkIO $ poller context.shutdownFlag
      return ()
    Nothing -> return ()

runWebServer :: BaseContext -> IO ()
runWebServer context = runWebServerFactory context getSentryIdentity loggingMiddleware webApi webServer

runWorker :: MVar () -> BaseContext -> IO ()
runWorker shutdownFlag context =
  worker runAppContextWithBaseContext runAppContextWithBaseContext'' shutdownFlag context workers permanentWorker
