module Application
  ( runApplication
  ) where

import Control.Lens ((^.))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (liftIO)
import System.IO

import Bootstrap.Config
import Bootstrap.Database
import Bootstrap.DatabaseMigration
import Bootstrap.Web
import Constant.ASCIIArt
import Constant.Component
import Constant.Resource
import LensesConfig
import Model.Context.BaseContext
import Service.Config.ApplicationConfigService
import Service.Config.BuildInfoConfigService
import Util.Logger

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
        let baseContext =
              BaseContext
              { _baseContextAppConfig = appConfig
              , _baseContextBuildInfoConfig = buildInfoConfig
              , _baseContextPool = dbPool
              }
        liftIO $ runDBMigrations baseContext
        liftIO $ runWebServer baseContext
