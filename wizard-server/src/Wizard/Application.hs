module Wizard.Application (
  runApplication,
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Reader (liftIO)
import Data.Foldable (forM_)
import System.Exit
import System.IO

import Shared.Common.Bootstrap.Config
import Shared.Common.Bootstrap.HttpClient
import Shared.Common.Bootstrap.Postgres
import Shared.Common.Bootstrap.S3
import Shared.Common.Constant.Component
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Service.Config.BuildInfoConfigService
import Wizard.Bootstrap.DatabaseMigration
import Wizard.Bootstrap.MetamodelMigration
import Wizard.Bootstrap.RegistryClient
import Wizard.Bootstrap.ServerCache
import Wizard.Bootstrap.Web
import Wizard.Bootstrap.Worker
import Wizard.Constant.ASCIIArt
import Wizard.Constant.Resource
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.Server.ServerConfigService
import Wizard.Util.Logger

runApplication :: IO ()
runApplication = do
  hSetBuffering stdout LineBuffering
  putStrLn asciiLogo
  serverConfig <- loadConfig serverConfigFile getServerConfig
  buildInfoConfig <- loadConfig buildInfoFile getBuildInfoConfig
  result <-
    runLogging serverConfig.logging.level $ do
      logInfo _CMP_ENVIRONMENT $ "set to " ++ show serverConfig.general.environment
      shutdownFlag <- liftIO newEmptyMVar
      dbPool <- connectPostgresDB serverConfig.logging serverConfig.database
      httpClientManager <- setupHttpClientManager serverConfig.logging
      s3Client <- setupS3Client serverConfig.s3 httpClientManager
      httpClientManager <- setupHttpClientManager serverConfig.logging
      registryClient <- setupRegistryClient serverConfig httpClientManager
      cache <- setupServerCache serverConfig
      let baseContext =
            BaseContext
              { serverConfig = serverConfig
              , buildInfoConfig = buildInfoConfig
              , dbPool = dbPool
              , s3Client = s3Client
              , httpClientManager = httpClientManager
              , registryClient = registryClient
              , shutdownFlag = shutdownFlag
              , cache = cache
              }
      result <- liftIO $ runDBMigrations baseContext
      case result of
        Just error -> return . Just $ error
        Nothing -> do
          liftIO $ runMetamodelMigrations baseContext
          liftIO $
            race_ (takeMVar shutdownFlag) (concurrently (runWebServer baseContext) (worker shutdownFlag baseContext))
          return Nothing
  forM_ result die
