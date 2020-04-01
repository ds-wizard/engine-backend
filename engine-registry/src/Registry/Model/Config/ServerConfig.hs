module Registry.Model.Config.ServerConfig where

import GHC.Generics
import Shared.Model.Config.Environment

data ServerConfig =
  ServerConfig
    { _serverConfigGeneral :: ServerConfigGeneral
    , _serverConfigDatabase :: ServerConfigDatabase
    , _serverConfigMail :: ServerConfigMail
    , _serverConfigAnalytics :: ServerConfigAnalytics
    }
  deriving (Generic, Show)

data ServerConfigGeneral =
  ServerConfigGeneral
    { _serverConfigGeneralEnvironment :: Environment
    , _serverConfigGeneralClientUrl :: String
    , _serverConfigGeneralServerPort :: Int
    , _serverConfigGeneralTemplateFolder :: String
    , _serverConfigGeneralRemoteLocalizationUrl :: Maybe String
    }
  deriving (Generic, Show)

data ServerConfigDatabase =
  ServerConfigDatabase
    { _serverConfigDatabaseHost :: String
    , _serverConfigDatabaseDatabaseName :: String
    , _serverConfigDatabasePort :: Integer
    , _serverConfigDatabaseAuthEnabled :: Bool
    , _serverConfigDatabaseUsername :: Maybe String
    , _serverConfigDatabasePassword :: Maybe String
    }
  deriving (Generic, Show)

data ServerConfigMail =
  ServerConfigMail
    { _serverConfigMailEnabled :: Bool
    , _serverConfigMailName :: Maybe String
    , _serverConfigMailEmail :: Maybe String
    , _serverConfigMailHost :: Maybe String
    , _serverConfigMailPort :: Maybe Int
    , _serverConfigMailSsl :: Maybe Bool
    , _serverConfigMailAuthEnabled :: Maybe Bool
    , _serverConfigMailUsername :: Maybe String
    , _serverConfigMailPassword :: Maybe String
    }
  deriving (Generic, Show)

data ServerConfigAnalytics =
  ServerConfigAnalytics
    { _serverConfigAnalyticsEnabled :: Bool
    , _serverConfigAnalyticsEmail :: Maybe String
    }
  deriving (Generic, Show)
