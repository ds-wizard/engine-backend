module Common.DSWConfig where

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
  { _acrDefaultRole :: Role
  , _acrAdmin :: [Permission]
  , _acrDataSteward :: [Permission]
  , _acrResearcher :: [Permission]
  }

data AppConfigMail = AppConfigMail
  { _acmName :: String
  , _acmEmail :: String
  , _acmHost :: String
  , _acmUsername :: String
  , _acmPassword :: String
  }

data BuildInfo = BuildInfo
  { _biAppName :: String
  , _biAppVersion :: String
  , _biBuiltAt :: String
  }

data DSWConfig = DSWConfig
  { _dswcfgWebConfig :: AppConfigWeb
  , _dswcfgDatabaseConfig :: AppConfigDatabase
  , _dswcfgJwtConfig :: AppConfigJwt
  , _dswcfgRoles :: AppConfigRoles
  , _dswcfgMail :: AppConfigMail
  , _dswcfgBuildInfo :: BuildInfo
  }

makeLenses ''AppConfigWeb

makeLenses ''AppConfigDatabase

makeLenses ''AppConfigJwt

makeLenses ''AppConfigRoles

makeLenses ''AppConfigMail

makeLenses ''BuildInfo

makeLenses ''DSWConfig

loadDSWConfig :: FilePath -> FilePath -> IO (Either CPError DSWConfig)
loadDSWConfig applicationConfigFile buildInfoFile = do
  file <- getDataFileName "" :: IO FilePath
  runExceptT $ do
    appConfigParser <- join $ liftIO $ readfile emptyCP applicationConfigFile
    buildInfoConfigParser <- join $ liftIO $ readfile emptyCP buildInfoFile
    webConfig <- loadAppConfigWeb appConfigParser
    databaseConfig <- loadAppConfigDatabase appConfigParser
    jwtConfig <- loadAppConfigJwt appConfigParser
    appRoles <- loadAppConfigRole appConfigParser
    appMail <- loadAppConfigMail appConfigParser
    buildInfo <- loadBuildInfo buildInfoConfigParser
    return
      DSWConfig
      { _dswcfgWebConfig = webConfig
      , _dswcfgDatabaseConfig = databaseConfig
      , _dswcfgJwtConfig = jwtConfig
      , _dswcfgRoles = appRoles
      , _dswcfgMail = appMail
      , _dswcfgBuildInfo = buildInfo
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
      defaultRole <- get configParser "Role" "defaultrole"
      adminPermissions <- get configParser "Role" "admin"
      dataStewardPermissions <- get configParser "Role" "datasteward"
      researcherPermissions <- get configParser "Role" "researcher"
      return
        AppConfigRoles
        { _acrDefaultRole = defaultRole
        , _acrAdmin = parseList adminPermissions
        , _acrDataSteward = parseList dataStewardPermissions
        , _acrResearcher = parseList researcherPermissions
        }
    loadAppConfigMail configParser = do
      mailName <- get configParser "Mail" "name"
      mailEmail <- get configParser "Mail" "email"
      mailHost <- get configParser "Mail" "host"
      mailUsername <- get configParser "Mail" "username"
      mailPassword <- get configParser "Mail" "password"
      return
        AppConfigMail
        { _acmName = mailName
        , _acmEmail = mailEmail
        , _acmHost = mailHost
        , _acmUsername = mailUsername
        , _acmPassword = mailPassword
        }
    loadBuildInfo configParser = do
      appName <- get configParser "DEFAULT" "name"
      appVersion <- get configParser "DEFAULT" "version"
      buildTimestamp <- get configParser "DEFAULT" "builtat"
      return BuildInfo {_biAppName = appName, _biAppVersion = appVersion, _biBuiltAt = buildTimestamp}
    parseList :: String -> [String]
    parseList listString = T.unpack <$> (T.splitOn ", " (T.pack listString))
