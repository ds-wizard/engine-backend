module Wizard.Application
  ( runApplication
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import System.IO

import LensesConfig
import Shared.Bootstrap.Config
import Shared.Bootstrap.HttpClient
import Shared.Bootstrap.Postgres
import Shared.Bootstrap.S3
import Shared.Constant.Component
import Shared.Service.Config.BuildInfoConfigService
import Wizard.Bootstrap.DatabaseMigration
import Wizard.Bootstrap.Localization
import Wizard.Bootstrap.MetamodelMigration
import Wizard.Bootstrap.RegistryClient
import Wizard.Bootstrap.ServerCache
import Wizard.Bootstrap.Web
import Wizard.Bootstrap.Worker
import Wizard.Constant.ASCIIArt
import Wizard.Constant.Resource
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.ServerConfigService
import Wizard.Util.Logger

runApplication :: IO ()
runApplication = do
  hSetBuffering stdout LineBuffering
  putStrLn asciiLogo
  serverConfig <- loadConfig serverConfigFile getServerConfig
  buildInfoConfig <- loadConfig buildInfoFile getBuildInfoConfig
  runLogging (serverConfig ^. logging . level) $ do
    logInfo _CMP_ENVIRONMENT $ "set to " ++ show (serverConfig ^. general . environment)
    shutdownFlag <- liftIO newEmptyMVar
    dbPool <- connectPostgresDB (serverConfig ^. logging) (serverConfig ^. database)
    httpClientManager <- setupHttpClientManager (serverConfig ^. logging)
    s3Client <- setupS3Client (serverConfig ^. s3) httpClientManager
    httpClientManager <- setupHttpClientManager (serverConfig ^. logging)
    registryClient <- setupRegistryClient serverConfig httpClientManager
    localization <- loadLocalization serverConfig
    cache <- setupServerCache serverConfig
    let baseContext =
          BaseContext
            { _baseContextServerConfig = serverConfig
            , _baseContextLocalization = localization
            , _baseContextBuildInfoConfig = buildInfoConfig
            , _baseContextDbPool = dbPool
            , _baseContextS3Client = s3Client
            , _baseContextHttpClientManager = httpClientManager
            , _baseContextRegistryClient = registryClient
            , _baseContextShutdownFlag = shutdownFlag
            , _baseContextCache = cache
            }
    liftIO $ runDBMigrations baseContext
    liftIO $ runMetamodelMigrations baseContext
    liftIO $ race_ (takeMVar shutdownFlag) (concurrently (runWebServer baseContext) (worker shutdownFlag baseContext))
    return ()
