module Service.Config.ConfigLoader where

import Control.Monad.Except
import Data.ConfigFile
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (lookupEnv)

import Model.Config.AppConfig

getOptional configParser section option
  | has_option configParser section option = get configParser section option >>= return . Just
  | otherwise = return Nothing

doIf True action _ = action
doIf False _ dummyValue = return dummyValue

loadDSWConfig :: FilePath -> FilePath -> IO (Either CPError AppConfig)
loadDSWConfig applicationConfigFile buildInfoFile = do
  runExceptT $ do
    appConfigParser <- join $ liftIO $ readfile emptyCP applicationConfigFile
    buildInfoConfigParser <- join $ liftIO $ readfile emptyCP buildInfoFile
    environment <- loadAppConfigEnvironment appConfigParser
    clientConfig <- loadAppConfigClient appConfigParser
    webConfig <- loadAppConfigWeb appConfigParser
    databaseConfig <- loadAppConfigDatabase appConfigParser
    messagingConfig <- loadAppConfigMessaging appConfigParser
    jwtConfig <- loadAppConfigJwt appConfigParser
    appRoles <- loadAppConfigRole appConfigParser
    appMail <- loadAppConfigMail appConfigParser
    appAnalytics <- loadAppConfigAnalytics appConfigParser
    appFeedback <- loadAppConfigFeedback appConfigParser
    buildInfo <- loadBuildInfo buildInfoConfigParser
    return
      AppConfig
      { _appConfigEnvironment = environment
      , _appConfigClientConfig = clientConfig
      , _appConfigWebConfig = webConfig
      , _appConfigDatabaseConfig = databaseConfig
      , _appConfigMessagingConfig = messagingConfig
      , _appConfigJwtConfig = jwtConfig
      , _appConfigRoles = appRoles
      , _appConfigMail = appMail
      , _appConfigAnalytics = appAnalytics
      , _appConfigFeedback = appFeedback
      , _appConfigBuildInfo = buildInfo
      }
  where
    loadAppConfigEnvironment configParser = do
      env <- get configParser "Environment" "env"
      return AppConfigEnvironment {_appConfigEnvironmentEnv = env}
    loadAppConfigClient configParser = do
      address <- get configParser "Client" "address"
      return AppConfigClient {_appConfigClientAddress = address}
    loadAppConfigWeb configParser = do
      webPort <- get configParser "Web" "port"
      serviceToken <- get configParser "Web" "servicetoken"
      return AppConfigWeb {_appConfigWebPort = webPort, _appConfigWebServiceToken = serviceToken}
    loadAppConfigDatabase configParser = do
      host <- get configParser "Database" "host"
      dbname <- get configParser "Database" "dbname"
      port <- get configParser "Database" "port"
      authEnabled <- get configParser "Database" "authenabled"
      username <- get configParser "Database" "username"
      password <- get configParser "Database" "password"
      return
        AppConfigDatabase
        { _appConfigDatabaseHost = host
        , _appConfigDatabaseDatabaseName = dbname
        , _appConfigDatabasePort = port
        , _appConfigDatabaseAuthEnabled = authEnabled
        , _appConfigDatabaseUsername = username
        , _appConfigDatabasePassword = password
        }
    loadAppConfigMessaging configParser = do
      host <- get configParser "Messaging" "host"
      port <- get configParser "Messaging" "port"
      username <- get configParser "Messaging" "username"
      password <- get configParser "Messaging" "password"
      return
        AppConfigMessaging
        { _appConfigMessagingHost = host
        , _appConfigMessagingPort = port
        , _appConfigMessagingUsername = username
        , _appConfigMessagingPassword = password
        }
    loadAppConfigJwt configParser = do
      jwtSecret <- get configParser "JWT" "secret"
      jwtVersion <- get configParser "JWT" "version"
      jwtExpiration <- get configParser "JWT" "expiration"
      return
        AppConfigJwt
        {_appConfigJwtSecret = jwtSecret, _appConfigJwtVersion = jwtVersion, _appConfigJwtExpiration = jwtExpiration}
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
      mailName <- doIf mailEnabled (get configParser "Mail" "name") ""
      mailEmail <- doIf mailEnabled (get configParser "Mail" "email") ""
      mailHost <- doIf mailEnabled (get configParser "Mail" "host") ""
      mailPort <- doIf mailEnabled (getOptional configParser "Mail" "port") Nothing
      mailSSL <- doIf mailEnabled (getOptional configParser "Mail" "ssl") Nothing
      mailUsername <- doIf mailEnabled (get configParser "Mail" "username") ""
      mailPassword <- doIf mailEnabled (get configParser "Mail" "password") ""
      return
        AppConfigMail
        { _appConfigMailEnabled = mailEnabled
        , _appConfigMailName = mailName
        , _appConfigMailEmail = mailEmail
        , _appConfigMailHost = mailHost
        , _appConfigMailPort = mailPort
        , _appConfigMailSsl = fromMaybe False mailSSL
        , _appConfigMailUsername = mailUsername
        , _appConfigMailPassword = mailPassword
        }
    loadAppConfigAnalytics configParser = do
      analyticsEnabled <- get configParser "Analytics" "enabled"
      analyticsEmail <- doIf analyticsEnabled (get configParser "Analytics" "email") ""
      return
        AppConfigAnalytics {_appConfigAnalyticsEnabled = analyticsEnabled, _appConfigAnalyticsEmail = analyticsEmail}
    loadAppConfigFeedback configParser = do
      feedbackTokenFromConfig <- get configParser "Feedback" "token"
      feedbackTokenFromEnv <- liftIO $ lookupEnv "FEEDBACK_TOKEN"
      let feedbackToken = fromMaybe feedbackTokenFromConfig feedbackTokenFromEnv
      feedbackOwner <- get configParser "Feedback" "owner"
      feedbackRepo <- get configParser "Feedback" "repo"
      feedbackIssueUrl <- get configParser "Feedback" "issueurl"
      return
        AppConfigFeedback
        { _appConfigFeedbackToken = feedbackToken
        , _appConfigFeedbackOwner = feedbackOwner
        , _appConfigFeedbackRepo = feedbackRepo
        , _appConfigFeedbackIssueUrl = feedbackIssueUrl
        }
    loadBuildInfo configParser = do
      appName <- get configParser "DEFAULT" "name"
      appVersion <- get configParser "DEFAULT" "version"
      buildTimestamp <- get configParser "DEFAULT" "builtat"
      return
        BuildInfo {_buildInfoAppName = appName, _buildInfoAppVersion = appVersion, _buildInfoBuiltAt = buildTimestamp}
    parseList :: String -> [String]
    parseList listString = T.unpack <$> (T.splitOn ", " (T.pack listString))
