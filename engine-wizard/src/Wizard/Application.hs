module Wizard.Application
  ( runApplication
  ) where

import Control.Lens ((^.))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (liftIO)
import System.IO

import Wizard.Bootstrap.Config
import Wizard.Bootstrap.Database
import Wizard.Bootstrap.DatabaseMigration
import Wizard.Bootstrap.HttpClient
import Wizard.Bootstrap.Localization
import Wizard.Bootstrap.Messaging
import Wizard.Bootstrap.MetamodelMigration
import Wizard.Bootstrap.Web
import Wizard.Constant.ASCIIArt
import Wizard.Constant.Component
import Wizard.Constant.Resource
import Wizard.LensesConfig
import Wizard.Model.Context.BaseContext
import Wizard.Service.Config.ApplicationConfigService
import Wizard.Service.Config.BuildInfoConfigService
import Wizard.Util.Logger

runApplication :: IO ()
runApplication = do
  hSetBuffering stdout LineBuffering
  runStdoutLoggingT $ do
    liftIO $ putStrLn asciiLogo
    logInfo $ msg _CMP_SERVER "started"
    hLoadConfig applicationConfigFile getApplicationConfig $ \appConfig ->
      hLoadConfig buildInfoFile getBuildInfoConfig $ \buildInfoConfig -> do
        logInfo $ "ENVIRONMENT: set to " ++ (show $ appConfig ^. general . environment)
        dbPool <- connectDB appConfig
        msgChannel <- connectMQ appConfig
        httpClientManager <- setupHttpClientManager appConfig
        localization <- loadLocalization appConfig
        let baseContext =
              BaseContext
                { _baseContextAppConfig = appConfig
                , _baseContextLocalization = localization
                , _baseContextBuildInfoConfig = buildInfoConfig
                , _baseContextPool = dbPool
                , _baseContextMsgChannel = msgChannel
                , _baseContextHttpClientManager = httpClientManager
                }
        liftIO $ runDBMigrations baseContext
        liftIO $ runMetamodelMigrations baseContext
        liftIO $ runWebServer baseContext
