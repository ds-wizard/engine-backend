module Application where

import Control.Lens ((^.))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (liftIO, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Default (def)
import Network.Wai.Handler.Warp
       (Settings, defaultSettings, setPort)
import Web.Scotty.Trans (Options, scottyOptsT, settings, verbose)

import Api.Router
import Constant.Component
import Database.Connection
import qualified Database.Migration.Development.Migration as DM
import qualified Database.Migration.Production.Migration as PM
import LensesConfig
import Model.Config.Environment
import Model.Context.AppContextHelpers
import Model.Context.BaseContext
import Service.Config.ConfigLoader
import Util.Logger

applicationConfigFile = "config/app-config.cfg"

buildInfoFile = "config/build-info.cfg"

runServer :: IO ()
runServer =
  runStdoutLoggingT $ do
    liftIO $
      putStrLn
        "/-------------------------------------------------------------\\\n\
  \|    _____   _____ _____     _____                            |\n\
  \|   |  __ \\ / ____|  __ \\   / ____|                           |\n\
  \|   | |  | | (___ | |__) | | (___   ___ _ ____   _____ _ __   |\n\
  \|   | |  | |\\___ \\|  ___/   \\___ \\ / _ \\ '__\\ \\ / / _ \\ '__|  |\n\
  \|   | |__| |____) | |       ____) |  __/ |   \\ V /  __/ |     |   \n\
  \|   |_____/|_____/|_|      |_____/ \\___|_|    \\_/ \\___|_|     |   \n\
  \|                                                             |\n\
  \\\-------------------------------------------------------------/"
    logInfo $ msg _CMP_SERVER "started"
    eitherDspConfig <- liftIO $ loadDSWConfig applicationConfigFile buildInfoFile
    case eitherDspConfig of
      Left (errorDate, reason) -> do
        logError $ msg _CMP_CONFIG "load failed"
        logError $ msg _CMP_CONFIG "Can't load app-config.cfg or build-info.cfg. Maybe the file is missing or not well-formatted"
        logError $ msg _CMP_CONFIG (show errorDate)
      Right dswConfig -> do
        logInfo $ msg _CMP_CONFIG "loaded"
        logInfo $ "ENVIRONMENT: set to " ++ (show $ dswConfig ^. environment . env)
        runStdoutLoggingT $ createDBConn dswConfig $ \dbPool -> do
          lift . logInfo $ msg _CMP_DATABASE "connected"
          let serverPort = dswConfig ^. webConfig ^. port
          let baseContext = BaseContext {_baseContextConfig = dswConfig, _baseContextPool = dbPool}
          liftIO $ runDBMigrations baseContext
          liftIO $ runApplication baseContext

runDBMigrations context =
  case context ^. config . environment . env of
    Development -> runStdoutLoggingT $ runAppContextWithBaseContext DM.runMigration context
    Staging -> runStdoutLoggingT $ PM.runMigration context
    Production -> runStdoutLoggingT $ PM.runMigration context
    _ -> return ()

runApplication :: BaseContext -> IO ()
runApplication context = do
  let o = getOptions context
  let r m = runStdoutLoggingT $ runReaderT (runBaseContextM m) context
  scottyOptsT o r (createEndpoints context)

getOptions :: BaseContext -> Options
getOptions context =
  def
  { settings = getSettings context
  , verbose =
      case context ^. config . environment . env of
        Production -> 0
        Staging -> 1
        Development -> 1
        Test -> 0
  }

getSettings :: BaseContext -> Settings
getSettings context =
  let webPort = context ^. config . webConfig . port
  in setPort webPort defaultSettings
