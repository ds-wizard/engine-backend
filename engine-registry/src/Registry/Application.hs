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
import Registry.Service.Config.ServerConfigService
import Registry.Util.Logger
import Shared.Service.Config.BuildInfoConfigService

runApplication :: IO ()
runApplication = do
  hSetBuffering stdout LineBuffering
  runStdoutLoggingT $ do
    liftIO $ putStrLn asciiLogo
    logInfo _CMP_SERVER "started"
    hLoadConfig serverConfigFile getServerConfig $ \serverConfig ->
      hLoadConfig buildInfoFile getBuildInfoConfig $ \buildInfoConfig -> do
        logInfo _CMP_ENVIRONMENT $ "set to " ++ show (serverConfig ^. general . environment)
        dbPool <- connectDB (serverConfig ^. database)
        localization <- loadLocalization serverConfig
        let baseContext =
              BaseContext
                { _baseContextServerConfig = serverConfig
                , _baseContextLocalization = localization
                , _baseContextBuildInfoConfig = buildInfoConfig
                , _baseContextPool = dbPool
                }
        liftIO $ runDBMigrations baseContext
        liftIO $ runWebServer baseContext
