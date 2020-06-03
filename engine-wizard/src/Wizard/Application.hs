module Wizard.Application
  ( runApplication
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens ((^.))
import Control.Monad (forever)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (liftIO)
import System.IO

import LensesConfig
import Shared.Service.Config.BuildInfoConfigService
import Wizard.Bootstrap.Config
import Wizard.Bootstrap.Database
import Wizard.Bootstrap.DatabaseMigration
import Wizard.Bootstrap.HttpClient
import Wizard.Bootstrap.Localization
import Wizard.Bootstrap.Messaging
import Wizard.Bootstrap.MetamodelMigration
import Wizard.Bootstrap.RegistryClient
import Wizard.Bootstrap.Web
import Wizard.Constant.ASCIIArt
import Wizard.Constant.Component
import Wizard.Constant.Resource
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.ServerConfigService
import Wizard.Util.Logger

runApplication :: IO ()
runApplication = do
  hSetBuffering stdout LineBuffering
  forever . runStdoutLoggingT $ do
    shutdownFlag <- liftIO newEmptyMVar
    liftIO $ putStrLn asciiLogo
    logInfo _CMP_SERVER "started"
    hLoadConfig serverConfigFile getServerConfig $ \serverConfig ->
      hLoadConfig buildInfoFile getBuildInfoConfig $ \buildInfoConfig -> do
        logInfo _CMP_ENVIRONMENT $ "set to " ++ show (serverConfig ^. general . environment)
        dbPool <- connectDB (serverConfig ^. database)
        msgChannel <- connectMQ serverConfig
        httpClientManager <- setupHttpClientManager serverConfig
        registryClient <- setupRegistryClient serverConfig httpClientManager
        localization <- loadLocalization serverConfig
        let baseContext =
              BaseContext
                { _baseContextServerConfig = serverConfig
                , _baseContextLocalization = localization
                , _baseContextBuildInfoConfig = buildInfoConfig
                , _baseContextPool = dbPool
                , _baseContextMsgChannel = msgChannel
                , _baseContextHttpClientManager = httpClientManager
                , _baseContextRegistryClient = registryClient
                , _baseContextShutdownFlag = shutdownFlag
                }
        liftIO $ runDBMigrations baseContext
        liftIO $ runMetamodelMigrations baseContext
        liftIO $ race_ (takeMVar shutdownFlag) (runWebServer baseContext)
