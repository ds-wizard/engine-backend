module Shared.Common.Application where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Reader (liftIO)
import Data.Foldable (forM_)
import System.Exit
import System.IO

import Shared.Common.Bootstrap.Config
import Shared.Common.Bootstrap.DatabaseMigration
import Shared.Common.Bootstrap.HttpClient
import Shared.Common.Bootstrap.Postgres
import Shared.Common.Bootstrap.S3
import Shared.Common.Constant.Component
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Service.Config.BuildInfo.BuildInfoConfigService
import Shared.Common.Service.Config.Server.ServerConfigService
import Shared.Common.Util.Logger

runWebServerWithWorkers
  asciiLogo
  serverConfigFile
  validateServerConfig
  buildInfoFile
  createBaseContext
  prodDBMigrations
  runDevDBMigrations
  afterDbMigrationHook
  runWebServer
  worker =
    do
      hSetBuffering stdout LineBuffering
      putStrLn asciiLogo
      serverConfig <- loadConfig serverConfigFile (getServerConfig validateServerConfig)
      buildInfoConfig <- loadConfig buildInfoFile getBuildInfoConfig
      result <-
        runLogging serverConfig.logging.level $ do
          logInfo _CMP_ENVIRONMENT $ "set to " ++ serverConfig.general.environment
          shutdownFlag <- liftIO newEmptyMVar
          dbPool <- connectPostgresDB serverConfig.logging serverConfig.database
          httpClientManager <- setupHttpClientManager serverConfig.logging
          s3Client <- setupS3Client serverConfig.s3 httpClientManager
          baseContext <- createBaseContext serverConfig buildInfoConfig dbPool s3Client httpClientManager shutdownFlag
          result <- liftIO $ runDBMigration baseContext prodDBMigrations runDevDBMigrations
          case result of
            Just error -> return . Just $ error
            Nothing -> do
              liftIO $ afterDbMigrationHook baseContext
              liftIO $ race_ (takeMVar shutdownFlag) (concurrently (runWebServer baseContext) (worker shutdownFlag baseContext))
              return Nothing
      forM_ result die
