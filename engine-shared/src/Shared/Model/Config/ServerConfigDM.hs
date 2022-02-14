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

defaultMail :: ServerConfigMail
defaultMail =
  ServerConfigMail
    { _serverConfigMailEnabled = True
    , _serverConfigMailName = "DS Wizard"
    , _serverConfigMailEmail = ""
    , _serverConfigMailHost = ""
    , _serverConfigMailPort = 465
    , _serverConfigMailSsl = False
    , _serverConfigMailAuthEnabled = False
    , _serverConfigMailUsername = ""
    , _serverConfigMailPassword = ""
    }

defaultAnalytics :: ServerConfigAnalytics
defaultAnalytics = ServerConfigAnalytics {_serverConfigAnalyticsEnabled = False, _serverConfigAnalyticsEmail = ""}

defaultLogging :: ServerConfigLogging
defaultLogging =
  ServerConfigLogging {_serverConfigLoggingLevel = LevelInfo, _serverConfigLoggingHttpClientDebug = False}

defaultCloud :: ServerConfigCloud
defaultCloud = ServerConfigCloud {_serverConfigCloudEnabled = False, _serverConfigCloudDomain = Nothing}
