module Model.Config.DSWConfig where

import Common.Types

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
  }

data AppConfigJwt = AppConfigJwt
  { _appConfigJwtSecret :: String
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

data BuildInfo = BuildInfo
  { _buildInfoAppName :: String
  , _buildInfoAppVersion :: String
  , _buildInfoBuiltAt :: String
  }

data DSWConfig = DSWConfig
  { _dSWConfigClientConfig :: AppConfigClient
  , _dSWConfigWebConfig :: AppConfigWeb
  , _dSWConfigDatabaseConfig :: AppConfigDatabase
  , _dSWConfigJwtConfig :: AppConfigJwt
  , _dSWConfigRoles :: AppConfigRoles
  , _dSWConfigMail :: AppConfigMail
  , _dSWConfigBuildInfo :: BuildInfo
  }
