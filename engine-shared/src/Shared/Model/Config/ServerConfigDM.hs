module Shared.Model.Config.ServerConfigDM where

import Control.Monad.Logger (LogLevel(..))

import Shared.Model.Config.ServerConfig

defaultDatabase :: ServerConfigDatabase
defaultDatabase =
  ServerConfigDatabase
    { _serverConfigDatabaseConnectionString = "postgresql://posgres:posgres@postgres:5432/engine-wizard"
    , _serverConfigDatabaseStripeSize = 1
    , _serverConfigDatabaseConnectionTimeout = 10
    , _serverConfigDatabaseMaxConnections = 10
    }

defaultS3 :: ServerConfigS3
defaultS3 =
  ServerConfigS3
    { _serverConfigS3Url = "http://minio:9000"
    , _serverConfigS3PublicUrl = "https://s3.ds-wizard.org"
    , _serverConfigS3Username = "minioadmin"
    , _serverConfigS3Password = "minioadmin"
    , _serverConfigS3Bucket = "engine-wizard"
    , _serverConfigS3Region = Nothing
    }

defaultAnalytics :: ServerConfigAnalytics
defaultAnalytics = ServerConfigAnalytics {_serverConfigAnalyticsEnabled = False, _serverConfigAnalyticsEmail = ""}

defaultSentry :: ServerConfigSentry
defaultSentry = ServerConfigSentry {_serverConfigSentryEnabled = False, _serverConfigSentryDsn = ""}

defaultLogging :: ServerConfigLogging
defaultLogging =
  ServerConfigLogging
    { _serverConfigLoggingLevel = LevelInfo
    , _serverConfigLoggingHttpClientDebug = False
    , _serverConfigLoggingWebsocketDebug = False
    }

defaultCloud :: ServerConfigCloud
defaultCloud = ServerConfigCloud {_serverConfigCloudEnabled = False, _serverConfigCloudDomain = Nothing}
