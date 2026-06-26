module Wizard.Model.Config.ServerConfigDM where

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Config.ServerConfigDM
import Shared.Worker.Model.Worker.CronWorker
import Wizard.Model.Config.ServerConfig
import Wizard.Worker.CronWorkers
import WizardLib.Public.Model.Config.ServerConfigDM

defaultConfig :: ServerConfig
defaultConfig =
  ServerConfig
    { general = defaultGeneral
    , database = defaultDatabase
    , s3 = defaultS3
    , aws = defaultAws
    , sentry = defaultSentry
    , userEmailLink = defaultUserEmailLink
    , cache = defaultCache
    , document = defaultDocument
    , externalLink = defaultExternalLink
    , feedback = defaultFeedback
    , knowledgeModelEditor = defaultKnowledgeModelEditor
    , project = defaultProject
    , temporaryFile = defaultTemporaryFile
    , userToken = defaultUserToken
    , analyticalMails = defaultAnalyticalMails
    , logging = defaultLogging
    , cloud = defaultCloud
    , persistentCommand = defaultPersistentCommand
    , signalBridge = defaultSignalBridge
    , admin = defaultAdmin
    , registry = defaultRegistry
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

defaultRegistrySyncJob :: ServerConfigCronWorker
defaultRegistrySyncJob =
  ServerConfigCronWorker {enabled = True, cron = registrySyncWorker.cronDefault}

defaultUserEmailLink :: ServerConfigUserEmailLink
defaultUserEmailLink = ServerConfigUserEmailLink {clean = defaultUserEmailLinkClean}

defaultUserEmailLinkClean :: ServerConfigCronWorker
defaultUserEmailLinkClean =
  ServerConfigCronWorker {enabled = True, cron = userEmailLinkWorker.cronDefault}

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

defaultProject :: ServerConfigProject
defaultProject =
  ServerConfigProject
    { clean = defaultProjectClean
    , squash = defaultProjectSquash
    , assigneeNotification = defaultProjectAssigneeNotification
    }

defaultProjectClean :: ServerConfigCronWorker
defaultProjectClean =
  ServerConfigCronWorker {enabled = True, cron = cleanProjectWorker.cronDefault}

defaultProjectSquash :: ServerConfigCronWorker
defaultProjectSquash =
  ServerConfigCronWorker {enabled = True, cron = squashProjectEventsWorker.cronDefault}

defaultProjectAssigneeNotification :: ServerConfigCronWorker
defaultProjectAssigneeNotification =
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
    , setProjectArn = ""
    , addEventArn = ""
    , addFileArn = ""
    , logOutAllArn = ""
    }

defaultAdmin :: ServerConfigAdmin
defaultAdmin =
  ServerConfigAdmin
    { enabled = False
    , serverUrl = ""
    }

defaultRegistry :: ServerConfigRegistry
defaultRegistry =
  ServerConfigRegistry
    { url = "https://api.registry.ds-wizard.org"
    , clientUrl = "https://registry.ds-wizard.org"
    , sync = defaultRegistrySyncJob
    }
