module Shared.Model.Config.ServerConfigDM where

import Control.Monad.Logger (LogLevel(..))

import Shared.Model.Config.ServerConfig

defaultDatabase :: ServerConfigDatabase
defaultDatabase =
  ServerConfigDatabase
    { _serverConfigDatabaseHost = "mongo"
    , _serverConfigDatabaseDatabaseName = "wizard-server"
    , _serverConfigDatabasePort = 27017
    , _serverConfigDatabaseAuthEnabled = False
    , _serverConfigDatabaseUsername = ""
    , _serverConfigDatabasePassword = ""
    , _serverConfigDatabaseConnectionPoolSize = 1
    , _serverConfigDatabaseStripeSize = 1
    , _serverConfigDatabaseConnectionIdleTime = 1
    , _serverConfigDatabaseConnectionString = ""
    , _serverConfigDatabaseConnectionTimeout = 30000
    , _serverConfigDatabaseMaxConnections = 10
    }

defaultS3 :: ServerConfigS3
defaultS3 =
  ServerConfigS3
    { _serverConfigS3Url = "http://minio:9000"
    , _serverConfigS3Username = "minioadmin"
    , _serverConfigS3Password = "minioadmin"
    , _serverConfigS3Bucket = "engine-registry"
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
