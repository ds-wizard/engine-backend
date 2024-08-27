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
    , useDevMigration = False
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

defaultAws :: ServerConfigAws
defaultAws =
  ServerConfigAws
    { awsAccessKeyId = ""
    , awsSecretAccessKey = ""
    , awsRegion = ""
    , awsRole = ""
    }

defaultSentry :: ServerConfigSentry
defaultSentry = ServerConfigSentry {enabled = False, dsn = ""}

defaultJwt :: ServerConfigJwt
defaultJwt = ServerConfigJwt {expiration = 14 * 24}

defaultAnalyticalMails :: ServerConfigAnalyticalMails
defaultAnalyticalMails = ServerConfigAnalyticalMails {enabled = False, email = ""}

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
    , signalBridgeUrl = Nothing
    }

defaultPlan :: ServerConfigPlan
defaultPlan = ServerConfigPlan {recomputeJob = defaultPlanRecomputeJob}

defaultPlanRecomputeJob :: ServerConfigCronWorker
defaultPlanRecomputeJob =
  ServerConfigCronWorker {enabled = False, cron = "0 * * * *"}

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
