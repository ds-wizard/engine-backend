module Wizard.Model.Config.ServerConfigDM where

import Shared.Common.Model.Config.Environment
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Config.ServerConfigDM
import Shared.Worker.Model.Worker.CronWorker
import Wizard.Model.Config.ServerConfig
import Wizard.Worker.CronWorkers

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
    , actionKey = defaultActionKey
    , branch = defaultBranch
    , cache = defaultCache
    , document = defaultDocument
    , feedback = defaultFeedback
    , persistentCommand = defaultPersistentCommand
    , plan = defaultPlan
    , questionnaire = defaultQuestionnaire
    , temporaryFile = defaultTemporaryFile
    , userToken = defaultUserToken
    , logging = defaultLogging
    , cloud = defaultCloud
    , admin = defaultAdmin
    , signalBridge = defaultSignalBridge
    , modules = defaultModules
    , aws = defaultAws
    }

defaultGeneral :: ServerConfigGeneral
defaultGeneral =
  ServerConfigGeneral
    { environment = Production
    , clientUrl = ""
    , serverPort = 3000
    , secret = ""
    , rsaPrivateKey = undefined
    , integrationConfig = "wizard-server/config/integration.yml"
    }

defaultJwt :: ServerConfigJwt
defaultJwt = ServerConfigJwt {expiration = 14 * 24}

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
        , "QTN_ACTION_PERM"
        , "QTN_IMPORTER_PERM"
        , "QTN_TML_PERM"
        , "DOC_TML_READ_PERM"
        , "CFG_PERM"
        , "SUBM_PERM"
        , "DOC_TML_WRITE_PERM"
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
        , "QTN_ACTION_PERM"
        , "QTN_IMPORTER_PERM"
        , "QTN_TML_PERM"
        , "DOC_TML_READ_PERM"
        , "SUBM_PERM"
        , "DOC_TML_WRITE_PERM"
        ]
    , researcher = ["PM_READ_PERM", "QTN_PERM", "DOC_TML_READ_PERM", "SUBM_PERM"]
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
  ServerConfigCronWorker {enabled = True, cron = registrySyncWorker.cronDefault}

defaultActionKey :: ServerConfigActionKey
defaultActionKey = ServerConfigActionKey {clean = defaultActionKeyClean}

defaultActionKeyClean :: ServerConfigCronWorker
defaultActionKeyClean =
  ServerConfigCronWorker {enabled = True, cron = actionKeyWorker.cronDefault}

defaultBranch :: ServerConfigBranch
defaultBranch = ServerConfigBranch {squash = defaultBranchSquash}

defaultBranchSquash :: ServerConfigCronWorker
defaultBranchSquash =
  ServerConfigCronWorker {enabled = True, cron = squashBranchEventsWorker.cronDefault}

defaultCache :: ServerConfigCache
defaultCache =
  ServerConfigCache
    { dataExpiration = 14 * 24
    , websocketExpiration = 24
    , purgeExpired = defaultCachePurgeExpired
    }

defaultCachePurgeExpired :: ServerConfigCronWorker
defaultCachePurgeExpired =
  ServerConfigCronWorker {enabled = True, cron = cacheWorker.cronDefault}

defaultDocument :: ServerConfigDocument
defaultDocument = ServerConfigDocument {clean = defaultDocumentClean}

defaultDocumentClean :: ServerConfigCronWorker
defaultDocumentClean =
  ServerConfigCronWorker {enabled = True, cron = documentWorker.cronDefault}

defaultFeedback :: ServerConfigFeedback
defaultFeedback =
  ServerConfigFeedback
    { apiUrl = "https://api.github.com"
    , webUrl = "https://github.com"
    , sync = defaultFeedbackSync
    }

defaultFeedbackSync :: ServerConfigCronWorker
defaultFeedbackSync =
  ServerConfigCronWorker {enabled = True, cron = feedbackWorker.cronDefault}

defaultPlan :: ServerConfigPlan
defaultPlan = ServerConfigPlan {recomputeJob = defaultPlanRecomputeJob}

defaultPlanRecomputeJob :: ServerConfigCronWorker
defaultPlanRecomputeJob =
  ServerConfigCronWorker {enabled = False, cron = tenantPlanWorker.cronDefault}

defaultQuestionnaire :: ServerConfigQuestionnaire
defaultQuestionnaire =
  ServerConfigQuestionnaire
    { clean = defaultQuestionnaireClean
    , squash = defaultQuestionnaireSquash
    }

defaultQuestionnaireClean :: ServerConfigCronWorker
defaultQuestionnaireClean =
  ServerConfigCronWorker {enabled = True, cron = cleanQuestionnaireWorker.cronDefault}

defaultQuestionnaireSquash :: ServerConfigCronWorker
defaultQuestionnaireSquash =
  ServerConfigCronWorker {enabled = True, cron = squashQuestionnaireEventsWorker.cronDefault}

defaultTemporaryFile :: ServerConfigTemporaryFile
defaultTemporaryFile = ServerConfigTemporaryFile {clean = defaultTemporaryFileClean}

defaultTemporaryFileClean :: ServerConfigCronWorker
defaultTemporaryFileClean =
  ServerConfigCronWorker {enabled = True, cron = temporaryFileWorker.cronDefault}

defaultUserToken :: ServerConfigUserToken
defaultUserToken = ServerConfigUserToken {clean = defaultUserTokenClean, expire = defaultUserTokenExpire}

defaultUserTokenClean :: ServerConfigCronWorker
defaultUserTokenClean =
  ServerConfigCronWorker {enabled = True, cron = cleanUserTokenWorker.cronDefault}

defaultUserTokenExpire :: ServerConfigCronWorker
defaultUserTokenExpire =
  ServerConfigCronWorker {enabled = True, cron = expireUserTokenWorker.cronDefault}

defaultAdmin :: ServerConfigAdmin
defaultAdmin =
  ServerConfigAdmin {enabled = False}

defaultSignalBridge :: ServerConfigSignalBridge
defaultSignalBridge =
  ServerConfigSignalBridge
    { enabled = False
    , updatePermsArn = ""
    , updateUserGroupArn = ""
    , setQuestionnaireArn = ""
    , logOutAllArn = ""
    }

defaultModules :: ServerConfigModules
defaultModules =
  ServerConfigModules
    { wizard = defaultModule
    , admin = defaultModule
    , integrationHub = defaultModule
    , reporting = defaultModule
    , guide = defaultModule
    }

defaultModule :: ServerConfigModule
defaultModule =
  ServerConfigModule {title = "", description = "", icon = "", url = Nothing}
