module Wizard.Application where

import Control.Concurrent (MVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Client (Manager)
import Network.Minio (MinioConn)

import Shared.Common.Application
import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Model.Config.ServerConfig
import Wizard.Bootstrap.MetamodelMigration
import Wizard.Bootstrap.RegistryClient
import Wizard.Bootstrap.ServerCache
import Wizard.Bootstrap.Web
import Wizard.Bootstrap.Worker
import Wizard.Constant.ASCIIArt
import Wizard.Constant.Resource
import qualified Wizard.Database.Migration.Development.Migration as DevDB
import qualified Wizard.Database.Migration.Production.Migration as ProdDB
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.Server.ServerConfigValidation

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
  registryClient <- setupRegistryClient serverConfig httpClientManager
  cache <- setupServerCache serverConfig
  return BaseContext {..}

afterDbMigrationHook :: BaseContext -> IO ()
afterDbMigrationHook = runMetamodelMigrations
