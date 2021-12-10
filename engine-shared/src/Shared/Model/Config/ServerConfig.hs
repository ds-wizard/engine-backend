module Shared.Model.Config.ServerConfig where

import Control.Monad.Logger (LogLevel(..))
import GHC.Generics

data ServerConfigDatabase =
  ServerConfigDatabase
    { _serverConfigDatabaseConnectionString :: String
    , _serverConfigDatabaseStripeSize :: Int
    , _serverConfigDatabaseConnectionTimeout :: Int
    , _serverConfigDatabaseMaxConnections :: Int
    }
  deriving (Generic, Show)

data ServerConfigS3 =
  ServerConfigS3
    { _serverConfigS3Url :: String
    , _serverConfigS3PublicUrl :: String
    , _serverConfigS3Username :: String
    , _serverConfigS3Password :: String
    , _serverConfigS3Bucket :: String
    , _serverConfigS3Region :: Maybe String
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

data ServerConfigExperimental =
  ServerConfigExperimental
    { _serverConfigExperimentalMoreAppsEnabled :: Bool
    }
  deriving (Generic, Show)
