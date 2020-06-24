module Wizard.Application
  ( runApplication
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import System.IO

import LensesConfig
import Shared.Constant.Component
import Shared.Service.Config.BuildInfoConfigService
import Wizard.Bootstrap.Config
import Wizard.Bootstrap.Database
import Wizard.Bootstrap.DatabaseMigration
import Wizard.Bootstrap.HttpClient
import Wizard.Bootstrap.Localization
import Wizard.Bootstrap.Messaging
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
    dbPool <- connectDB serverConfig
    msgChannel <- connectMQ serverConfig
    httpClientManager <- setupHttpClientManager serverConfig
    registryClient <- setupRegistryClient serverConfig httpClientManager
    localization <- loadLocalization serverConfig
    cache <- setupServerCache serverConfig
    let baseContext =
          BaseContext
            { _baseContextServerConfig = serverConfig
            , _baseContextLocalization = localization
            , _baseContextBuildInfoConfig = buildInfoConfig
            , _baseContextPool = dbPool
            , _baseContextMsgChannel = msgChannel
            , _baseContextHttpClientManager = httpClientManager
            , _baseContextRegistryClient = registryClient
            , _baseContextCache = cache
            }
    liftIO $ runDBMigrations baseContext
    liftIO $ runMetamodelMigrations baseContext
    liftIO $ race_ (takeMVar shutdownFlag) (concurrently (runWebServer baseContext) (cronJob shutdownFlag baseContext))
    return ()
