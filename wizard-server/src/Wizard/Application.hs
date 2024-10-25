module Wizard.Application where

import Control.Concurrent
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)

import Shared.Common.Application
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

runApplication :: IO ()
runApplication =
  runWebServerWithWorkers
    asciiLogo
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
afterDbMigrationHook _ = return ()

runWebServer :: BaseContext -> IO ()
runWebServer context = runWebServerFactory context getSentryIdentity loggingMiddleware webApi webServer

runWorker :: MVar () -> BaseContext -> IO ()
runWorker shutdownFlag context =
  worker runAppContextWithBaseContext runAppContextWithBaseContext'' shutdownFlag context workers permanentWorker
