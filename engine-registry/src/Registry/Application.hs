module Registry.Application
  ( runApplication
  ) where

import Control.Lens ((^.))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (liftIO)
import System.IO

import LensesConfig
import Registry.Bootstrap.Config
import Registry.Bootstrap.Database
import Registry.Bootstrap.DatabaseMigration
import Registry.Bootstrap.Localization
import Registry.Bootstrap.Web
import Registry.Constant.ASCIIArt
import Registry.Constant.Component
import Registry.Constant.Resource
import Registry.Model.Context.BaseContext
import Registry.Service.Config.ApplicationConfigService
import Registry.Service.Config.BuildInfoConfigService
import Registry.Util.Logger

runApplication :: IO ()
runApplication = do
  hSetBuffering stdout LineBuffering
  runStdoutLoggingT $ do
    liftIO $ putStrLn asciiLogo
    logInfo $ msg _CMP_SERVER "started"
    hLoadConfig applicationConfigFile getApplicationConfig $ \appConfig ->
      hLoadConfig buildInfoFile getBuildInfoConfig $ \buildInfoConfig -> do
        logInfo $ "ENVIRONMENT: set to " ++ show (appConfig ^. general . environment)
        dbPool <- connectDB appConfig
        localization <- loadLocalization appConfig
        let baseContext =
              BaseContext
                { _baseContextAppConfig = appConfig
                , _baseContextLocalization = localization
                , _baseContextBuildInfoConfig = buildInfoConfig
                , _baseContextPool = dbPool
                }
        liftIO $ runDBMigrations baseContext
        liftIO $ runWebServer baseContext
