module Shared.Common.Model.Config.ServerConfigDM where

import Control.Monad.Logger (LogLevel (..))

import Shared.Common.Model.Config.ServerConfig

defaultDatabase :: ServerConfigDatabase
defaultDatabase =
  ServerConfigDatabase
    { connectionString = "postgresql://posgres:posgres@postgres:5432/wizard-server"
    , stripeSize = 1
    , connectionTimeout = 10
    , maxConnections = 50
    , vacuumCleaner = defaultDatabaseVacuumCleaner
    }

defaultDatabaseVacuumCleaner :: ServerConfigDatabaseVacuumCleaner
defaultDatabaseVacuumCleaner =
  ServerConfigDatabaseVacuumCleaner
    { enabled = False
    , cron = "45 1 * * *"
    , tables = []
    }

defaultS3 :: ServerConfigS3
defaultS3 =
  ServerConfigS3
    { url = "http://minio:9000"
    , username = "minioadmin"
    , password = "minioadmin"
    , bucket = "wizard-server"
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

defaultPersistentCommand :: ServerConfigPersistentCommand
defaultPersistentCommand =
  ServerConfigPersistentCommand
    { lambdaFunctions = []
    , listenerJob = defaultPersistentCommandListenerJob
    , retryJob = defaultPersistentCommandRetryJob
    , retryLambdaJob = defaultPersistentCommandRetryLambdaJob
    }

defaultPersistentCommandListenerJob :: ServerConfigPersistentCommandListenerJob
defaultPersistentCommandListenerJob =
  ServerConfigPersistentCommandListenerJob {enabled = True}

defaultPersistentCommandRetryJob :: ServerConfigCronWorker
defaultPersistentCommandRetryJob =
  ServerConfigCronWorker {enabled = True, cron = "* * * * *"}

defaultPersistentCommandRetryLambdaJob :: ServerConfigCronWorker
defaultPersistentCommandRetryLambdaJob =
  ServerConfigCronWorker {enabled = True, cron = "* * * * *"}

defaultAws :: ServerConfigAws
defaultAws =
  ServerConfigAws
    { awsAccessKeyId = ""
    , awsSecretAccessKey = ""
    , awsRegion = ""
    }
