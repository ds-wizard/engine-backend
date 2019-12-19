module Registry.Model.Config.AppConfig where

import GHC.Generics
import Shared.Model.Config.Environment

data AppConfig =
  AppConfig
    { _appConfigGeneral :: AppConfigGeneral
    , _appConfigDatabase :: AppConfigDatabase
    , _appConfigMail :: AppConfigMail
    , _appConfigAnalytics :: AppConfigAnalytics
    }
  deriving (Generic, Show)

data AppConfigGeneral =
  AppConfigGeneral
    { _appConfigGeneralEnvironment :: Environment
    , _appConfigGeneralClientUrl :: String
    , _appConfigGeneralServerPort :: Int
    , _appConfigGeneralRemoteLocalizationUrl :: Maybe String
    }
  deriving (Generic, Show)

data AppConfigDatabase =
  AppConfigDatabase
    { _appConfigDatabaseHost :: String
    , _appConfigDatabaseDatabaseName :: String
    , _appConfigDatabasePort :: Integer
    , _appConfigDatabaseAuthEnabled :: Bool
    , _appConfigDatabaseUsername :: Maybe String
    , _appConfigDatabasePassword :: Maybe String
    }
  deriving (Generic, Show)

data AppConfigMail =
  AppConfigMail
    { _appConfigMailEnabled :: Bool
    , _appConfigMailName :: Maybe String
    , _appConfigMailEmail :: Maybe String
    , _appConfigMailHost :: Maybe String
    , _appConfigMailPort :: Maybe Int
    , _appConfigMailSsl :: Maybe Bool
    , _appConfigMailAuthEnabled :: Maybe Bool
    , _appConfigMailUsername :: Maybe String
    , _appConfigMailPassword :: Maybe String
    }
  deriving (Generic, Show)

data AppConfigAnalytics =
  AppConfigAnalytics
    { _appConfigAnalyticsEnabled :: Bool
    , _appConfigAnalyticsEmail :: Maybe String
    }
  deriving (Generic, Show)
