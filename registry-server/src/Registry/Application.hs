module Registry.Application (
  runApplication,
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Reader (liftIO)
import Data.Foldable (forM_)
import System.Exit
import System.IO

import Registry.Bootstrap.DatabaseMigration
import Registry.Bootstrap.Web
import Registry.Bootstrap.Worker
import Registry.Constant.ASCIIArt
import Registry.Constant.Resource
import Registry.Model.Config.ServerConfig
import Registry.Model.Config.ServerConfigIM ()
import Registry.Model.Config.ServerConfigJM ()
import Registry.Model.Context.BaseContext
import Registry.Service.Config.Server.ServerConfigValidation
import Shared.Common.Bootstrap.Config
import Shared.Common.Bootstrap.HttpClient
import Shared.Common.Bootstrap.Postgres
import Shared.Common.Bootstrap.S3
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Service.Config.BuildInfo.BuildInfoConfigService
import Shared.Common.Service.Config.Server.ServerConfigService
import Shared.Common.Util.Logger

runApplication :: IO ()
runApplication = do
  hSetBuffering stdout LineBuffering
  putStrLn asciiLogo
  serverConfig <- loadConfig serverConfigFile (getServerConfig validateServerConfig)
  buildInfoConfig <- loadConfig buildInfoFile getBuildInfoConfig
  result <-
    runLogging serverConfig.logging.level $ do
      logInfo _CMP_ENVIRONMENT $ "set to " ++ show serverConfig.general.environment
      shutdownFlag <- liftIO newEmptyMVar
      dbPool <- connectPostgresDB serverConfig.logging serverConfig.database
      httpClientManager <- setupHttpClientManager serverConfig.logging
      s3Client <- setupS3Client serverConfig.s3 httpClientManager
      let baseContext =
            BaseContext
              { serverConfig = serverConfig
              , buildInfoConfig = buildInfoConfig
              , dbPool = dbPool
              , s3Client = s3Client
              , httpClientManager = httpClientManager
              }
      result <- liftIO $ runDBMigrations baseContext
      case result of
        Just error -> return . Just $ error
        Nothing -> do
          liftIO $ race_ (takeMVar shutdownFlag) (concurrently (runWebServer baseContext) (runWorker shutdownFlag baseContext))
          return Nothing
  forM_ result die
