module Wizard.Model.Config.ServerConfigDM where

import Shared.Model.Config.Environment
import Shared.Model.Config.ServerConfigDM
import Wizard.Model.Config.ServerConfig

defaultConfig :: ServerConfig
defaultConfig =
  ServerConfig
    { _serverConfigGeneral = defaultGeneral
    , _serverConfigDatabase = defaultDatabase
    , _serverConfigS3 = defaultS3
    , _serverConfigMessaging = defaultMessaging
    , _serverConfigJwt = defaultJwt
    , _serverConfigRoles = defaultRoles
    , _serverConfigMail = defaultMail
    , _serverConfigRegistry = defaultRegistry
    , _serverConfigAnalytics = defaultAnalytics
    , _serverConfigBranch = defaultBranch
    , _serverConfigDocument = defaultDocument
    , _serverConfigFeedback = defaultFeedback
    , _serverConfigPersistentCommand = defaultPersistentCommand
    , _serverConfigQuestionnaire = defaultQuestionnaire
    , _serverConfigLogging = defaultLogging
    , _serverConfigCloud = defaultCloud
    }

defaultGeneral :: ServerConfigGeneral
defaultGeneral =
  ServerConfigGeneral
    { _serverConfigGeneralEnvironment = Production
    , _serverConfigGeneralClientUrl = ""
    , _serverConfigGeneralServerPort = 3000
    , _serverConfigGeneralServiceToken = ""
    , _serverConfigGeneralSecret = ""
    , _serverConfigGeneralIntegrationConfig = "engine-wizard/config/integration.yml"
    , _serverConfigGeneralTemplateFolder = "engine-wizard/"
    , _serverConfigGeneralRemoteLocalizationUrl = Nothing
    , _serverConfigGeneralClientStyleBuilderUrl = "http://wizard-style-builder:3002"
    }

defaultMessaging :: ServerConfigMessaging
defaultMessaging =
  ServerConfigMessaging
    { _serverConfigMessagingEnabled = True
    , _serverConfigMessagingHost = "rabbitmq"
    , _serverConfigMessagingPort = 5672
    , _serverConfigMessagingUsername = "guest"
    , _serverConfigMessagingPassword = "guest"
    , _serverConfigMessagingVhost = "/"
    }

defaultJwt :: ServerConfigJwt
defaultJwt = ServerConfigJwt {_serverConfigJwtVersion = 2, _serverConfigJwtExpiration = 14}

defaultRoles :: ServerConfigRoles
defaultRoles =
  ServerConfigRoles
    { _serverConfigRolesAdmin =
        [ "ADMIN_PERM"
        , "UM_PERM"
        , "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "QTN_TML_PERM"
        , "DMP_PERM"
        , "CFG_PERM"
        , "SUBM_PERM"
        , "TML_PERM"
        , "DOC_PERM"
        ]
    , _serverConfigRolesDataSteward =
        [ "KM_PERM"
        , "KM_UPGRADE_PERM"
        , "KM_PUBLISH_PERM"
        , "PM_READ_PERM"
        , "PM_WRITE_PERM"
        , "QTN_PERM"
        , "QTN_TML_PERM"
        , "DMP_PERM"
        , "SUBM_PERM"
        , "TML_PERM"
        ]
    , _serverConfigRolesResearcher = ["PM_READ_PERM", "QTN_PERM", "DMP_PERM", "SUBM_PERM"]
    }

defaultRegistry :: ServerConfigRegistry
defaultRegistry =
  ServerConfigRegistry
    { _serverConfigRegistryUrl = "https://api.registry.ds-wizard.org"
    , _serverConfigRegistryClientUrl = "https://registry.ds-wizard.org"
    }

defaultBranch :: ServerConfigBranch
defaultBranch = ServerConfigBranch {_serverConfigBranchSquash = defaultBranchSquash}

defaultBranchSquash :: ServerConfigCronWorker
defaultBranchSquash =
  ServerConfigCronWorker {_serverConfigCronWorkerEnabled = True, _serverConfigCronWorkerCron = "15 2 * * *"}

defaultDocument :: ServerConfigDocument
defaultDocument = ServerConfigDocument {_serverConfigDocumentClean = defaultDocumentClean}

defaultDocumentClean :: ServerConfigCronWorker
defaultDocumentClean =
  ServerConfigCronWorker {_serverConfigCronWorkerEnabled = True, _serverConfigCronWorkerCron = "0 */4 * * *"}

defaultFeedback :: ServerConfigFeedback
defaultFeedback =
  ServerConfigFeedback
    { _serverConfigFeedbackApiUrl = "https://api.github.com"
    , _serverConfigFeedbackWebUrl = "https://github.com"
    , _serverConfigFeedbackSync = defaultFeedbackSync
    }

defaultFeedbackSync :: ServerConfigCronWorker
defaultFeedbackSync =
  ServerConfigCronWorker {_serverConfigCronWorkerEnabled = True, _serverConfigCronWorkerCron = "0 2 * * *"}

defaultPersistentCommand :: ServerConfigPersistentCommand
defaultPersistentCommand =
  ServerConfigPersistentCommand {_serverConfigPersistentCommandRetryJob = defaultPersistentCommandRetryJob}

defaultPersistentCommandRetryJob :: ServerConfigCronWorker
defaultPersistentCommandRetryJob =
  ServerConfigCronWorker {_serverConfigCronWorkerEnabled = True, _serverConfigCronWorkerCron = "*/15 * * * *"}

defaultQuestionnaire :: ServerConfigQuestionnaire
defaultQuestionnaire =
  ServerConfigQuestionnaire
    { _serverConfigQuestionnaireClean = defaultQuestionnaireClean
    , _serverConfigQuestionnaireSquash = defaultQuestionnaireSquash
    }

defaultQuestionnaireClean :: ServerConfigCronWorker
defaultQuestionnaireClean =
  ServerConfigCronWorker {_serverConfigCronWorkerEnabled = True, _serverConfigCronWorkerCron = "15 */4 * * *"}

defaultQuestionnaireSquash :: ServerConfigCronWorker
defaultQuestionnaireSquash =
  ServerConfigCronWorker {_serverConfigCronWorkerEnabled = True, _serverConfigCronWorkerCron = "15 2 * * *"}
