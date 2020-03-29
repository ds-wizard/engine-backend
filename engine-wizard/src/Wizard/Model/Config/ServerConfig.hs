module Wizard.Model.Config.ServerConfig where

import GHC.Generics
import Shared.Model.Config.Environment
import Wizard.Model.User.User

data ServerConfig =
  ServerConfig
    { _serverConfigGeneral :: ServerConfigGeneral
    , _serverConfigDatabase :: ServerConfigDatabase
    , _serverConfigMessaging :: ServerConfigMessaging
    , _serverConfigJwt :: ServerConfigJwt
    , _serverConfigRoles :: ServerConfigRoles
    , _serverConfigMail :: ServerConfigMail
    , _serverConfigRegistry :: ServerConfigRegistry
    , _serverConfigAnalytics :: ServerConfigAnalytics
    , _serverConfigFeedback :: ServerConfigFeedback
    }
  deriving (Generic, Show)

data ServerConfigGeneral =
  ServerConfigGeneral
    { _serverConfigGeneralEnvironment :: Environment
    , _serverConfigGeneralClientUrl :: String
    , _serverConfigGeneralServerPort :: Int
    , _serverConfigGeneralServiceToken :: String
    , _serverConfigGeneralSecret :: String
    , _serverConfigGeneralIntegrationConfig :: String
    , _serverConfigGeneralTemplateFolder :: String
    , _serverConfigGeneralRemoteLocalizationUrl :: Maybe String
    , _serverConfigGeneralDebugLogHttpClient :: Bool
    }
  deriving (Generic, Show)

data ServerConfigDatabase =
  ServerConfigDatabase
    { _serverConfigDatabaseHost :: String
    , _serverConfigDatabaseDatabaseName :: String
    , _serverConfigDatabasePort :: Integer
    , _serverConfigDatabaseAuthEnabled :: Bool
    , _serverConfigDatabaseUsername :: String
    , _serverConfigDatabasePassword :: String
    }
  deriving (Generic, Show)

data ServerConfigMessaging =
  ServerConfigMessaging
    { _serverConfigMessagingEnabled :: Bool
    , _serverConfigMessagingHost :: String
    , _serverConfigMessagingPort :: Integer
    , _serverConfigMessagingUsername :: String
    , _serverConfigMessagingPassword :: String
    , _serverConfigMessagingVhost :: String
    }
  deriving (Generic, Show)

data ServerConfigJwt =
  ServerConfigJwt
    { _serverConfigJwtVersion :: Integer
    , _serverConfigJwtExpiration :: Integer
    }
  deriving (Generic, Show)

data ServerConfigRoles =
  ServerConfigRoles
    { _serverConfigRolesAdmin :: [Permission]
    , _serverConfigRolesDataSteward :: [Permission]
    , _serverConfigRolesResearcher :: [Permission]
    }
  deriving (Generic, Show)

data ServerConfigMail =
  ServerConfigMail
    { _serverConfigMailEnabled :: Bool
    , _serverConfigMailName :: String
    , _serverConfigMailEmail :: String
    , _serverConfigMailHost :: String
    , _serverConfigMailPort :: Int
    , _serverConfigMailSsl :: Bool
    , _serverConfigMailAuthEnabled :: Bool
    , _serverConfigMailUsername :: String
    , _serverConfigMailPassword :: String
    }
  deriving (Generic, Show)

data ServerConfigRegistry =
  ServerConfigRegistry
    { _serverConfigRegistryUrl :: String
    , _serverConfigRegistryClientUrl :: String
    }
  deriving (Generic, Show)

data ServerConfigAnalytics =
  ServerConfigAnalytics
    { _serverConfigAnalyticsEnabled :: Bool
    , _serverConfigAnalyticsEmail :: String
    }
  deriving (Generic, Show)

data ServerConfigFeedback =
  ServerConfigFeedback
    { _serverConfigFeedbackApiUrl :: String
    , _serverConfigFeedbackWebUrl :: String
    }
  deriving (Generic, Show)
