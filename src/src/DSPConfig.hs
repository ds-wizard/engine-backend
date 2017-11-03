module DSPConfig where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.ConfigFile
import Data.Text

import Paths_src

applicationConfigFile = "config/app-config.cfg"

buildInfoFile = "config/build-info.cfg"

data AppConfigWeb = AppConfigWeb
  { _acwPort :: Int
  }

data AppConfigDatabase = AppConfigDatabase
  { _acdbHost :: String
  , _acdbDatabaseName :: String
  , _acdbPort :: Integer
  }

data AppConfigJwt = AppConfigJwt
  { _acjwtSecret :: String
  }

data BuildInfo = BuildInfo
  { _biAppName :: String
  , _biAppVersion :: String
  , _biBuiltAt :: String
  }

data DSPConfig = DSPConfig
  { _dspcfgWebConfig :: AppConfigWeb
  , _dspcfgDatabaseConfig :: AppConfigDatabase
  , _dspcfgJwtConfig :: AppConfigJwt
  , _dspcfgBuildInfo :: BuildInfo
  }

makeLenses ''AppConfigWeb

makeLenses ''AppConfigDatabase

makeLenses ''AppConfigJwt

makeLenses ''BuildInfo

makeLenses ''DSPConfig

loadDSPConfig :: IO (Either CPError DSPConfig)
loadDSPConfig = do
  file <- getDataFileName "" :: IO FilePath
  runExceptT $ do
    appConfigParser <- join $ liftIO $ readfile emptyCP applicationConfigFile
    buildInfoConfigParser <- join $ liftIO $ readfile emptyCP buildInfoFile
    webConfig <- loadAppConfigWeb appConfigParser
    databaseConfig <- loadAppConfigDatabase appConfigParser
    jwtConfig <- loadAppConfigJwt appConfigParser
    buildInfo <- loadBuildInfo buildInfoConfigParser
    return
      DSPConfig
      { _dspcfgWebConfig = webConfig
      , _dspcfgDatabaseConfig = databaseConfig
      , _dspcfgJwtConfig = jwtConfig
      , _dspcfgBuildInfo = buildInfo
      }
  where
    loadAppConfigWeb configParser = do
      webPort <- get configParser "Web" "port"
      return AppConfigWeb {_acwPort = webPort}
    loadAppConfigDatabase configParser = do
      host <- get configParser "Database" "host"
      dbname <- get configParser "Database" "dbname"
      port <- get configParser "Database" "port"
      return
        AppConfigDatabase
        {_acdbHost = host, _acdbDatabaseName = dbname, _acdbPort = port}
    loadAppConfigJwt configParser = do
      jwtSecret <- get configParser "JWT" "secret"
      return AppConfigJwt {_acjwtSecret = jwtSecret}
    loadBuildInfo configParser = do
      appName <- get configParser "DEFAULT" "name"
      appVersion <- get configParser "DEFAULT" "version"
      buildTimestamp <- get configParser "DEFAULT" "builtat"
      return
        BuildInfo
        { _biAppName = appName
        , _biAppVersion = appVersion
        , _biBuiltAt = buildTimestamp
        }
