module Registry.Application where

import Control.Concurrent (MVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)

import Registry.Bootstrap.Web
import Registry.Bootstrap.Worker
import Registry.Constant.ASCIIArt
import Registry.Constant.Resource
import qualified Registry.Database.Migration.Development.Migration as DevDB
import qualified Registry.Database.Migration.Production.Migration as ProdDB
import Registry.Model.Config.ServerConfig
import Registry.Model.Config.ServerConfigIM ()
import Registry.Model.Config.ServerConfigJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.Config.Server.ServerConfigValidation
import Shared.Common.Application
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig

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
