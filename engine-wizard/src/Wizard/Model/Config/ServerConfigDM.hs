module Wizard.Model.Config.ServerConfigDM where

import Shared.Model.Config.Environment
import Shared.Model.Config.ServerConfigDM
import Wizard.Model.Config.ServerConfig

defaultConfig :: ServerConfig
defaultConfig =
  ServerConfig
    { general = defaultGeneral
    , database = defaultDatabase
    , s3 = defaultS3
    , jwt = defaultJwt
    , roles = defaultRoles
    , registry = defaultRegistry
    , analytics = defaultAnalytics
    , sentry = defaultSentry
    , branch = defaultBranch
    , cache = defaultCache
    , document = defaultDocument
    , feedback = defaultFeedback
    , persistentCommand = defaultPersistentCommand
    , plan = defaultPlan
    , questionnaire = defaultQuestionnaire
    , userToken = defaultUserToken
    , logging = defaultLogging
    , cloud = defaultCloud
    }

defaultGeneral :: ServerConfigGeneral
defaultGeneral =
  ServerConfigGeneral
    { environment = Production
    , clientUrl = ""
    , serverPort = 3000
    , secret = ""
    , integrationConfig = "engine-wizard/config/integration.yml"
    , clientStyleBuilderUrl = "http://wizard-style-builder:3002"
    }

defaultJwt :: ServerConfigJwt
defaultJwt = ServerConfigJwt {version = 3, expiration = 14}

defaultRoles :: ServerConfigRoles
defaultRoles =
  ServerConfigRoles
    { admin =
        [ "UM_PERM"
        , "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "QTN_IMPORTER_PERM"
        , "QTN_TML_PERM"
        , "DMP_PERM"
        , "CFG_PERM"
        , "SUBM_PERM"
        , "TML_PERM"
        , "DOC_PERM"
        , "LOC_PERM"
        ]
    , dataSteward =
        [ "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "QTN_IMPORTER_PERM"
        , "QTN_TML_PERM"
        , "DMP_PERM"
        , "SUBM_PERM"
        , "TML_PERM"
        ]
    , researcher = ["PM_READ_PERM", "QTN_PERM", "DMP_PERM", "SUBM_PERM"]
    }

defaultRegistry :: ServerConfigRegistry
defaultRegistry =
  ServerConfigRegistry
    { url = "https://api.registry.ds-wizard.org"
    , clientUrl = "https://registry.ds-wizard.org"
    , sync = defaultRegistrySyncJob
    }

defaultRegistrySyncJob :: ServerConfigCronWorker
defaultRegistrySyncJob =
  ServerConfigCronWorker {enabled = True, cron = "*/15 * * * *"}

defaultBranch :: ServerConfigBranch
defaultBranch = ServerConfigBranch {squash = defaultBranchSquash}

defaultBranchSquash :: ServerConfigCronWorker
defaultBranchSquash =
  ServerConfigCronWorker {enabled = True, cron = "15 2 * * *"}

defaultCache :: ServerConfigCache
defaultCache =
  ServerConfigCache
    { dataExpiration = 14 * 24
    , websocketExpiration = 24
    , purgeExpired = defaultCachePurgeExpired
    }

defaultCachePurgeExpired :: ServerConfigCronWorker
defaultCachePurgeExpired =
  ServerConfigCronWorker {enabled = True, cron = "45 * * * *"}

defaultDocument :: ServerConfigDocument
defaultDocument = ServerConfigDocument {clean = defaultDocumentClean}

defaultDocumentClean :: ServerConfigCronWorker
defaultDocumentClean =
  ServerConfigCronWorker {enabled = True, cron = "0 */4 * * *"}

defaultFeedback :: ServerConfigFeedback
defaultFeedback =
  ServerConfigFeedback
    { apiUrl = "https://api.github.com"
    , webUrl = "https://github.com"
    , sync = defaultFeedbackSync
    }

defaultFeedbackSync :: ServerConfigCronWorker
defaultFeedbackSync =
  ServerConfigCronWorker {enabled = True, cron = "0 2 * * *"}

defaultPersistentCommand :: ServerConfigPersistentCommand
defaultPersistentCommand =
  ServerConfigPersistentCommand
    { listenerJob = defaultPersistentCommandListenerJob
    , retryJob = defaultPersistentCommandRetryJob
    }

defaultPersistentCommandListenerJob :: ServerConfigPersistentCommandListenerJob
defaultPersistentCommandListenerJob =
  ServerConfigPersistentCommandListenerJob {enabled = True}

defaultPersistentCommandRetryJob :: ServerConfigCronWorker
defaultPersistentCommandRetryJob =
  ServerConfigCronWorker {enabled = True, cron = "* * * * *"}

defaultPlan :: ServerConfigPlan
defaultPlan = ServerConfigPlan {recomputeJob = defaultPlanRecomputeJob}

defaultPlanRecomputeJob :: ServerConfigCronWorker
defaultPlanRecomputeJob =
  ServerConfigCronWorker {enabled = True, cron = "0 * * * *"}

defaultQuestionnaire :: ServerConfigQuestionnaire
defaultQuestionnaire =
  ServerConfigQuestionnaire
    { clean = defaultQuestionnaireClean
    , recomputeIndication = defaultQuestionnaireRecomputeIndication
    , squash = defaultQuestionnaireSquash
    }

defaultQuestionnaireClean :: ServerConfigCronWorker
defaultQuestionnaireClean =
  ServerConfigCronWorker {enabled = True, cron = "15 */4 * * *"}

defaultQuestionnaireRecomputeIndication :: ServerConfigCronWorker
defaultQuestionnaireRecomputeIndication =
  ServerConfigCronWorker {enabled = True, cron = "20 1 * * *"}

defaultQuestionnaireSquash :: ServerConfigCronWorker
defaultQuestionnaireSquash =
  ServerConfigCronWorker {enabled = True, cron = "15 2 * * *"}

defaultUserToken :: ServerConfigUserToken
defaultUserToken = ServerConfigUserToken {clean = defaultUserTokenClean}

defaultUserTokenClean :: ServerConfigCronWorker
defaultUserTokenClean =
  ServerConfigCronWorker {enabled = True, cron = "0 3 * * *"}
