module Application where

import Control.Lens ((^.))
import Control.Monad.Logger
       (logError, logErrorSH, logInfo, runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (liftIO, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Default (def)
import Network.Wai.Handler.Warp
       (Settings, defaultSettings, setPort)
import Web.Scotty.Trans (Options, scottyOptsT, settings, verbose)

import Api.Handler.Common
import Api.Resource.Info.InfoDTO
import Api.Router
import Common.ConfigLoader
import Common.Context
import Database.Connection
import Database.Migration.Migration
import LensesConfig
import Model.Config.DSWConfig
import Model.Context.AppContext

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
    $(logInfo) "SERVER: started"
    eitherDspConfig <- liftIO $ loadDSWConfig applicationConfigFile buildInfoFile
    case eitherDspConfig of
      Left (errorDate, reason) -> do
        $(logError) "CONFIG: load failed"
        $(logError) "Can't load app-config.cfg or build-info.cfg. Maybe the file is missing or not well-formatted"
        $(logErrorSH) errorDate
      Right dswConfig -> do
        $(logInfo) "CONFIG: loaded"
        runStdoutLoggingT $ createDBConn dswConfig $ \dbPool -> do
          lift $ $(logInfo) "DATABASE: connected"
          let context = Context {_ctxDbPool = dbPool, _ctxConfig = Config}
          let serverPort = dswConfig ^. webConfig ^. port
          let appContext =
                AppContext
                { _appContextEnvironment = Development
                , _appContextConfig = dswConfig
                , _appContextPool = dbPool
                , _appContextOldContext = context
                }
          runMigration appContext
          liftIO $ runApplication appContext

runApplication :: AppContext -> IO ()
runApplication context = do
  let o = getOptions context
  let r m = runStdoutLoggingT $ runReaderT (runAppContextM m) context
  scottyOptsT o r (createEndpoints context)

getOptions :: AppContext -> Options
getOptions context =
  def
  { settings = getSettings context
  , verbose =
      case context ^. environment of
        Development -> 1
        Production -> 0
        Test -> 0
  }

getSettings :: AppContext -> Settings
getSettings context =
  let webPort = context ^. config . webConfig . port
  in setPort webPort defaultSettings
