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

data ServerConfigAnalytics =
  ServerConfigAnalytics
    { _serverConfigAnalyticsEnabled :: Bool
    , _serverConfigAnalyticsEmail :: String
    }
  deriving (Generic, Show)

data ServerConfigSentry =
  ServerConfigSentry
    { _serverConfigSentryEnabled :: Bool
    , _serverConfigSentryDsn :: String
    }
  deriving (Generic, Show)

data ServerConfigLogging =
  ServerConfigLogging
    { _serverConfigLoggingLevel :: LogLevel
    , _serverConfigLoggingHttpClientDebug :: Bool
    , _serverConfigLoggingWebsocketDebug :: Bool
    }
  deriving (Generic, Show)

data ServerConfigCloud =
  ServerConfigCloud
    { _serverConfigCloudEnabled :: Bool
    , _serverConfigCloudDomain :: Maybe String
    , _serverConfigCloudPublicRegistrationEnabled :: Bool
    }
  deriving (Generic, Show)
