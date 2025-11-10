module Wizard.Model.Config.ServerConfigDM where

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
    , aws = defaultAws
    , sentry = defaultSentry
    , jwt = defaultJwt
    , roles = defaultRoles
    , actionKey = defaultActionKey
    , cache = defaultCache
    , document = defaultDocument
    , feedback = defaultFeedback
    , knowledgeModelEditor = defaultKnowledgeModelEditor
    , questionnaire = defaultQuestionnaire
    , temporaryFile = defaultTemporaryFile
    , userToken = defaultUserToken
    , analyticalMails = defaultAnalyticalMails
    , logging = defaultLogging
    , cloud = defaultCloud
    , plan = defaultPlan
    , persistentCommand = defaultPersistentCommand
    , signalBridge = defaultSignalBridge
    , admin = defaultAdmin
    , registry = defaultRegistry
    , modules = defaultModules
    }

defaultGeneral :: ServerConfigGeneral
defaultGeneral =
  ServerConfigGeneral
    { environment = "Production"
    , clientUrl = ""
    , serverPort = 3000
    , secret = ""
    , rsaPrivateKey = undefined
    , integrationConfig = "wizard-server/config/integration.yml"
    }

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
        , "QTN_FILE_PERM"
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

defaultRegistrySyncJob :: ServerConfigCronWorker
defaultRegistrySyncJob =
  ServerConfigCronWorker {enabled = True, cron = registrySyncWorker.cronDefault}

defaultActionKey :: ServerConfigActionKey
defaultActionKey = ServerConfigActionKey {clean = defaultActionKeyClean}

defaultActionKeyClean :: ServerConfigCronWorker
defaultActionKeyClean =
  ServerConfigCronWorker {enabled = True, cron = actionKeyWorker.cronDefault}

defaultCache :: ServerConfigCache
defaultCache =
  ServerConfigCache
    { dataExpiration = 14 * 24
    , websocketExpiration = 24
    , purgeExpired = defaultCachePurgeExpired
    , dataEnabled = True
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

defaultKnowledgeModelEditor :: ServerConfigKnowledgeModelEditor
defaultKnowledgeModelEditor = ServerConfigKnowledgeModelEditor {squash = defaultKnowledgeModelEditorSquash}

defaultKnowledgeModelEditorSquash :: ServerConfigCronWorker
defaultKnowledgeModelEditorSquash =
  ServerConfigCronWorker {enabled = True, cron = squashKnowledgeModelEditorEventsWorker.cronDefault}

defaultQuestionnaire :: ServerConfigQuestionnaire
defaultQuestionnaire =
  ServerConfigQuestionnaire
    { clean = defaultQuestionnaireClean
    , squash = defaultQuestionnaireSquash
    , assigneeNotification = defaultQuestionnaireAssigneeNotification
    }

defaultQuestionnaireClean :: ServerConfigCronWorker
defaultQuestionnaireClean =
  ServerConfigCronWorker {enabled = True, cron = cleanQuestionnaireWorker.cronDefault}

defaultQuestionnaireSquash :: ServerConfigCronWorker
defaultQuestionnaireSquash =
  ServerConfigCronWorker {enabled = True, cron = squashQuestionnaireEventsWorker.cronDefault}

defaultQuestionnaireAssigneeNotification :: ServerConfigCronWorker
defaultQuestionnaireAssigneeNotification =
  ServerConfigCronWorker {enabled = True, cron = assigneeNotificationWorker.cronDefault}

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

defaultSignalBridge :: ServerConfigSignalBridge
defaultSignalBridge =
  ServerConfigSignalBridge
    { enabled = False
    , updatePermsArn = ""
    , updateUserGroupArn = ""
    , setQuestionnaireArn = ""
    , addFileArn = ""
    , logOutAllArn = ""
    }

defaultAdmin :: ServerConfigAdmin
defaultAdmin =
  ServerConfigAdmin {enabled = False}

defaultRegistry :: ServerConfigRegistry
defaultRegistry =
  ServerConfigRegistry
    { url = "https://api.registry.ds-wizard.org"
    , clientUrl = "https://registry.ds-wizard.org"
    , sync = defaultRegistrySyncJob
    }

defaultModules :: ServerConfigModules
defaultModules =
  ServerConfigModules
    { wizard = defaultModule
    , admin = defaultModule
    , integrationHub = defaultModule
    , analytics = defaultModule
    , guide = defaultModule
    }

defaultModule :: ServerConfigModule
defaultModule =
  ServerConfigModule {title = "", description = "", icon = "", url = Nothing}
