module Model.Config.AppConfig where

import Model.Config.Environment
import Model.User.User

data AppConfigEnvironment = AppConfigEnvironment
  { _appConfigEnvironmentEnv :: Environment
  }

data AppConfigClient = AppConfigClient
  { _appConfigClientAddress :: String
  }

data AppConfigWeb = AppConfigWeb
  { _appConfigWebPort :: Int
  , _appConfigWebServiceToken :: String
  }

data AppConfigDatabase = AppConfigDatabase
  { _appConfigDatabaseHost :: String
  , _appConfigDatabaseDatabaseName :: String
  , _appConfigDatabasePort :: Integer
  , _appConfigDatabaseAuthEnabled :: Bool
  , _appConfigDatabaseUsername :: String
  , _appConfigDatabasePassword :: String
  }

data AppConfigMessaging = AppConfigMessaging
  { _appConfigMessagingHost :: String
  , _appConfigMessagingPort :: Integer
  , _appConfigMessagingUsername :: String
  , _appConfigMessagingPassword :: String
  }

data AppConfigJwt = AppConfigJwt
  { _appConfigJwtSecret :: String
  , _appConfigJwtVersion :: Integer
  , _appConfigJwtExpiration :: Integer
  }

data AppConfigRoles = AppConfigRoles
  { _appConfigRolesDefaultRole :: Role
  , _appConfigRolesAdmin :: [Permission]
  , _appConfigRolesDataSteward :: [Permission]
  , _appConfigRolesResearcher :: [Permission]
  }

data AppConfigMail = AppConfigMail
  { _appConfigMailEnabled :: Bool
  , _appConfigMailName :: String
  , _appConfigMailEmail :: String
  , _appConfigMailHost :: String
  , _appConfigMailPort :: Maybe Int
  , _appConfigMailSsl :: Bool
  , _appConfigMailUsername :: String
  , _appConfigMailPassword :: String
  }

data AppConfigAnalytics = AppConfigAnalytics
  { _appConfigAnalyticsEnabled :: Bool
  , _appConfigAnalyticsEmail :: String
  }

data AppConfigFeedback = AppConfigFeedback
  { _appConfigFeedbackToken :: String
  , _appConfigFeedbackOwner :: String
  , _appConfigFeedbackRepo :: String
  , _appConfigFeedbackIssueUrl :: String
  }

data BuildInfo = BuildInfo
  { _buildInfoAppName :: String
  , _buildInfoAppVersion :: String
  , _buildInfoBuiltAt :: String
  }

data AppConfig = AppConfig
  { _appConfigEnvironment :: AppConfigEnvironment
  , _appConfigClientConfig :: AppConfigClient
  , _appConfigWebConfig :: AppConfigWeb
  , _appConfigDatabaseConfig :: AppConfigDatabase
  , _appConfigMessagingConfig :: AppConfigMessaging
  , _appConfigJwtConfig :: AppConfigJwt
  , _appConfigRoles :: AppConfigRoles
  , _appConfigMail :: AppConfigMail
  , _appConfigAnalytics :: AppConfigAnalytics
  , _appConfigFeedback :: AppConfigFeedback
  , _appConfigBuildInfo :: BuildInfo
  }
