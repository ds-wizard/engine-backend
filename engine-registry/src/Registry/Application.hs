module Registry.Application (
  runApplication,
) where

import Control.Monad.Reader (liftIO)
import Data.Foldable (forM_)
import System.Exit
import System.IO

import Registry.Bootstrap.DatabaseMigration
import Registry.Bootstrap.Localization
import Registry.Bootstrap.Web
import Registry.Constant.ASCIIArt
import Registry.Constant.Resource
import Registry.Model.Config.ServerConfig
import Registry.Model.Context.BaseContext
import Registry.Service.Config.ServerConfigService
import Registry.Util.Logger
import Shared.Bootstrap.Config
import Shared.Bootstrap.HttpClient
import Shared.Bootstrap.Postgres
import Shared.Bootstrap.S3
import Shared.Model.Config.ServerConfig
import Shared.Service.Config.BuildInfoConfigService

runApplication :: IO ()
runApplication = do
  hSetBuffering stdout LineBuffering
  putStrLn asciiLogo
  serverConfig <- loadConfig serverConfigFile getServerConfig
  buildInfoConfig <- loadConfig buildInfoFile getBuildInfoConfig
  result <-
    runLogging serverConfig.logging.level $ do
      logInfo _CMP_ENVIRONMENT $ "set to " ++ show serverConfig.general.environment
      dbPool <- connectPostgresDB serverConfig.logging serverConfig.database
      httpClientManager <- setupHttpClientManager serverConfig.logging
      s3Client <- setupS3Client serverConfig.s3 httpClientManager
      localization <- loadLocalization serverConfig
      let baseContext =
            BaseContext
              { serverConfig = serverConfig
              , localization = localization
              , buildInfoConfig = buildInfoConfig
              , dbPool = dbPool
              , s3Client = s3Client
              }
      result <- liftIO $ runDBMigrations baseContext
      case result of
        Just error -> return . Just $ error
        Nothing -> do
          liftIO $ runWebServer baseContext
          return Nothing
  forM_ result die
