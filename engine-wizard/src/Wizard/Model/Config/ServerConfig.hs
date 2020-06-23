module Wizard.Model.Config.ServerConfig where

import GHC.Generics
import Shared.Model.Config.Environment
import Shared.Model.Config.ServerConfig
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
    , _serverConfigLogging :: ServerConfigLogging
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

data ServerConfigRegistry =
  ServerConfigRegistry
    { _serverConfigRegistryUrl :: String
    , _serverConfigRegistryClientUrl :: String
    }
  deriving (Generic, Show)

data ServerConfigFeedback =
  ServerConfigFeedback
    { _serverConfigFeedbackApiUrl :: String
    , _serverConfigFeedbackWebUrl :: String
    }
  deriving (Generic, Show)
