module Registry.Application where

import Control.Concurrent (MVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)

import Registry.Api.Middleware.LoggingMiddleware
import Registry.Api.Sentry
import Registry.Api.Web
import Registry.Constant.ASCIIArt
import Registry.Constant.Resource
import qualified Registry.Database.Migration.Development.Migration as DevDB
import qualified Registry.Database.Migration.Production.Migration as ProdDB
import Registry.Model.Config.ServerConfig
import Registry.Model.Config.ServerConfigIM ()
import Registry.Model.Config.ServerConfigJM ()
import Registry.Model.Context.BaseContext
import Registry.Model.Context.ContextMappers
import Registry.Service.Config.Server.ServerConfigValidation
import Registry.Worker.CronWorkers
import Registry.Worker.PermanentWorkers
import Shared.Common.Application
import Shared.Common.Bootstrap.Web
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig
import Shared.Worker.Bootstrap.Worker

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
createBaseContext serverConfig buildInfoConfig dbPool s3Client httpClientManager shutdownFlag = return BaseContext {..}

afterDbMigrationHook :: BaseContext -> IO ()
afterDbMigrationHook _ = return ()

runWebServer :: BaseContext -> IO ()
runWebServer context = runWebServerFactory context getSentryIdentity loggingMiddleware webApi webServer

runWorker :: MVar () -> BaseContext -> IO ()
runWorker shutdownFlag context =
  worker runAppContextWithBaseContext runAppContextWithBaseContext'' shutdownFlag context workers permanentWorker
