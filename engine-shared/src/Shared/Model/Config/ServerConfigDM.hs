module Shared.Model.Config.ServerConfigDM where

import Control.Monad.Logger (LogLevel (..))

import Shared.Model.Config.ServerConfig

defaultDatabase :: ServerConfigDatabase
defaultDatabase =
  ServerConfigDatabase
    { connectionString = "postgresql://posgres:posgres@postgres:5432/engine-wizard"
    , stripeSize = 1
    , connectionTimeout = 10
    , maxConnections = 50
    }

defaultS3 :: ServerConfigS3
defaultS3 =
  ServerConfigS3
    { url = "http://minio:9000"
    , publicUrl = "https://s3.ds-wizard.org"
    , username = "minioadmin"
    , password = "minioadmin"
    , bucket = "engine-wizard"
    , region = Nothing
    }

defaultAnalytics :: ServerConfigAnalytics
defaultAnalytics = ServerConfigAnalytics {enabled = False, email = ""}

defaultSentry :: ServerConfigSentry
defaultSentry = ServerConfigSentry {enabled = False, dsn = ""}

defaultLogging :: ServerConfigLogging
defaultLogging =
  ServerConfigLogging
    { level = LevelInfo
    , httpClientDebug = False
    , websocketDebug = False
    }

defaultCloud :: ServerConfigCloud
defaultCloud =
  ServerConfigCloud
    { enabled = False
    , domain = Nothing
    , publicRegistrationEnabled = False
    }
