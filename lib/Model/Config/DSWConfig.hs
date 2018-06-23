module Model.Config.DSWConfig where

import Common.Types

data Environment
  = Production
  | Staging
  | Development
  | Test
  deriving (Eq, Read, Show)

data AppConfigEnvironment = AppConfigEnvironment
  { _appConfigEnvironmentEnv :: Environment
  }

data AppConfigClient = AppConfigClient
  { _appConfigClientAddress :: String
  }

data AppConfigWeb = AppConfigWeb
  { _appConfigWebPort :: Int
  }

data AppConfigDatabase = AppConfigDatabase
  { _appConfigDatabaseHost :: String
  , _appConfigDatabaseDatabaseName :: String
  , _appConfigDatabasePort :: Integer
  , _appConfigDatabaseAuthEnabled :: Bool
  , _appConfigDatabaseUsername :: String
  , _appConfigDatabasePassword :: String
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
  , _appConfigMailUsername :: String
  , _appConfigMailPassword :: String
  }

data AppConfigAnalytics = AppConfigAnalytics
  { _appConfigAnalyticsEnabled :: Bool
  , _appConfigAnalyticsEmail :: String
  }

data BuildInfo = BuildInfo
  { _buildInfoAppName :: String
  , _buildInfoAppVersion :: String
  , _buildInfoBuiltAt :: String
  }

data DSWConfig = DSWConfig
  { _dSWConfigEnvironment :: AppConfigEnvironment
  , _dSWConfigClientConfig :: AppConfigClient
  , _dSWConfigWebConfig :: AppConfigWeb
  , _dSWConfigDatabaseConfig :: AppConfigDatabase
  , _dSWConfigJwtConfig :: AppConfigJwt
  , _dSWConfigRoles :: AppConfigRoles
  , _dSWConfigMail :: AppConfigMail
  , _dSWConfigAnalytics :: AppConfigAnalytics
  , _dSWConfigBuildInfo :: BuildInfo
  }
