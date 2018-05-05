module Common.ConfigLoader where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.ConfigFile
import qualified Data.Text as T

import Common.Types
import LensesConfig
import Model.Config.DSWConfig
import Paths_src

loadDSWConfig :: FilePath -> FilePath -> IO (Either CPError DSWConfig)
loadDSWConfig applicationConfigFile buildInfoFile = do
  runExceptT $ do
    appConfigParser <- join $ liftIO $ readfile emptyCP applicationConfigFile
    buildInfoConfigParser <- join $ liftIO $ readfile emptyCP buildInfoFile
    clientConfig <- loadAppConfigClient appConfigParser
    webConfig <- loadAppConfigWeb appConfigParser
    databaseConfig <- loadAppConfigDatabase appConfigParser
    jwtConfig <- loadAppConfigJwt appConfigParser
    appRoles <- loadAppConfigRole appConfigParser
    appMail <- loadAppConfigMail appConfigParser
    buildInfo <- loadBuildInfo buildInfoConfigParser
    return
      DSWConfig
      { _dSWConfigClientConfig = clientConfig
      , _dSWConfigWebConfig = webConfig
      , _dSWConfigDatabaseConfig = databaseConfig
      , _dSWConfigJwtConfig = jwtConfig
      , _dSWConfigRoles = appRoles
      , _dSWConfigMail = appMail
      , _dSWConfigBuildInfo = buildInfo
      }
  where
    loadAppConfigClient configParser = do
      address <- get configParser "Client" "address"
      return AppConfigClient {_appConfigClientAddress = address}
    loadAppConfigWeb configParser = do
      webPort <- get configParser "Web" "port"
      return AppConfigWeb {_appConfigWebPort = webPort}
    loadAppConfigDatabase configParser = do
      host <- get configParser "Database" "host"
      dbname <- get configParser "Database" "dbname"
      port <- get configParser "Database" "port"
      authEnabled <- get configParser "Database" "authenabled"
      username <- get configParser "Database" "port"
      password <- get configParser "Database" "port"
      return
        AppConfigDatabase
        { _appConfigDatabaseHost = host
        , _appConfigDatabaseDatabaseName = dbname
        , _appConfigDatabasePort = port
        , _appConfigDatabaseAuthEnabled = authEnabled
        , _appConfigDatabaseUsername = username
        , _appConfigDatabasePassword = password
        }
    loadAppConfigJwt configParser = do
      jwtSecret <- get configParser "JWT" "secret"
      return AppConfigJwt {_appConfigJwtSecret = jwtSecret}
    loadAppConfigRole configParser = do
      defaultRole <- get configParser "Role" "defaultrole"
      adminPermissions <- get configParser "Role" "admin"
      dataStewardPermissions <- get configParser "Role" "datasteward"
      researcherPermissions <- get configParser "Role" "researcher"
      return
        AppConfigRoles
        { _appConfigRolesDefaultRole = defaultRole
        , _appConfigRolesAdmin = parseList adminPermissions
        , _appConfigRolesDataSteward = parseList dataStewardPermissions
        , _appConfigRolesResearcher = parseList researcherPermissions
        }
    loadAppConfigMail configParser = do
      mailEnabled <- get configParser "Mail" "enabled"
      mailName <- get configParser "Mail" "name"
      mailEmail <- get configParser "Mail" "email"
      mailHost <- get configParser "Mail" "host"
      mailUsername <- get configParser "Mail" "username"
      mailPassword <- get configParser "Mail" "password"
      return
        AppConfigMail
        { _appConfigMailEnabled = mailEnabled
        , _appConfigMailName = mailName
        , _appConfigMailEmail = mailEmail
        , _appConfigMailHost = mailHost
        , _appConfigMailUsername = mailUsername
        , _appConfigMailPassword = mailPassword
        }
    loadBuildInfo configParser = do
      appName <- get configParser "DEFAULT" "name"
      appVersion <- get configParser "DEFAULT" "version"
      buildTimestamp <- get configParser "DEFAULT" "builtat"
      return
        BuildInfo {_buildInfoAppName = appName, _buildInfoAppVersion = appVersion, _buildInfoBuiltAt = buildTimestamp}
    parseList :: String -> [String]
    parseList listString = T.unpack <$> (T.splitOn ", " (T.pack listString))
