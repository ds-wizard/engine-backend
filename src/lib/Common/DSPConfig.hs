module Common.DSPConfig where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.ConfigFile
import qualified Data.Text as T

import Common.Types
import Paths_src

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

data AppConfigRoles = AppConfigRoles
  { _acrAdmin :: [Permission]
  , _acrDataSteward :: [Permission]
  , _acrResearcher :: [Permission]
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
  , _dspcfgRoles :: AppConfigRoles
  , _dspcfgBuildInfo :: BuildInfo
  }

makeLenses ''AppConfigWeb

makeLenses ''AppConfigDatabase

makeLenses ''AppConfigJwt

makeLenses ''AppConfigRoles

makeLenses ''BuildInfo

makeLenses ''DSPConfig

loadDSPConfig :: FilePath -> FilePath -> IO (Either CPError DSPConfig)
loadDSPConfig applicationConfigFile buildInfoFile = do
  file <- getDataFileName "" :: IO FilePath
  runExceptT $ do
    appConfigParser <- join $ liftIO $ readfile emptyCP applicationConfigFile
    buildInfoConfigParser <- join $ liftIO $ readfile emptyCP buildInfoFile
    webConfig <- loadAppConfigWeb appConfigParser
    databaseConfig <- loadAppConfigDatabase appConfigParser
    jwtConfig <- loadAppConfigJwt appConfigParser
    appRoles <- loadAppConfigRole appConfigParser
    buildInfo <- loadBuildInfo buildInfoConfigParser
    return
      DSPConfig
      { _dspcfgWebConfig = webConfig
      , _dspcfgDatabaseConfig = databaseConfig
      , _dspcfgJwtConfig = jwtConfig
      , _dspcfgRoles = appRoles
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
      return AppConfigDatabase {_acdbHost = host, _acdbDatabaseName = dbname, _acdbPort = port}
    loadAppConfigJwt configParser = do
      jwtSecret <- get configParser "JWT" "secret"
      return AppConfigJwt {_acjwtSecret = jwtSecret}
    loadAppConfigRole configParser = do
      adminPermissions <- get configParser "Role" "admin"
      dataStewardPermissions <- get configParser "Role" "datasteward"
      researcherPermissions <- get configParser "Role" "researcher"
      return
        AppConfigRoles
        { _acrAdmin = parseList adminPermissions
        , _acrDataSteward = parseList dataStewardPermissions
        , _acrResearcher = parseList researcherPermissions
        }
    loadBuildInfo configParser = do
      appName <- get configParser "DEFAULT" "name"
      appVersion <- get configParser "DEFAULT" "version"
      buildTimestamp <- get configParser "DEFAULT" "builtat"
      return BuildInfo {_biAppName = appName, _biAppVersion = appVersion, _biBuiltAt = buildTimestamp}
    parseList :: String -> [String]
    parseList listString = T.unpack <$> (T.splitOn ", " (T.pack listString))
