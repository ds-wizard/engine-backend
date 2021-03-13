module Shared.Model.Config.ServerConfig where

import Control.Monad.Logger (LogLevel(..))
import GHC.Generics

data ServerConfigDatabase =
  ServerConfigDatabase
    { _serverConfigDatabaseHost :: String
    , _serverConfigDatabaseDatabaseName :: String
    , _serverConfigDatabasePort :: Integer
    , _serverConfigDatabaseAuthEnabled :: Bool
    , _serverConfigDatabaseUsername :: String
    , _serverConfigDatabasePassword :: String
    , _serverConfigDatabaseConnectionPoolSize :: Int
    , _serverConfigDatabaseStripeSize :: Int
    , _serverConfigDatabaseConnectionIdleTime :: Int
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

data ServerConfigAnalytics =
  ServerConfigAnalytics
    { _serverConfigAnalyticsEnabled :: Bool
    , _serverConfigAnalyticsEmail :: String
    }
  deriving (Generic, Show)

data ServerConfigLogging =
  ServerConfigLogging
    { _serverConfigLoggingLevel :: LogLevel
    , _serverConfigLoggingHttpClientDebug :: Bool
    }
  deriving (Generic, Show)
