module Wizard.Model.Config.ServerConfigIM where

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Config.ServerConfigIM
import Wizard.Model.Config.ServerConfig

instance FromEnv ServerConfig where
  applyEnv serverConfig = do
    general <- applyEnv serverConfig.general
    database <- applyEnv serverConfig.database
    s3 <- applyEnv serverConfig.s3
    jwt <- applyEnv serverConfig.jwt
    roles <- applyEnv serverConfig.roles
    registry <- applyEnv serverConfig.registry
    analytics <- applyEnv serverConfig.analytics
    sentry <- applyEnv serverConfig.sentry
    actionKey <- applyEnv serverConfig.actionKey
    branch <- applyEnv serverConfig.branch
    cache <- applyEnv serverConfig.cache
    document <- applyEnv serverConfig.document
    feedback <- applyEnv serverConfig.feedback
    persistentCommand <- applyEnv serverConfig.persistentCommand
    plan <- applyEnv serverConfig.plan
    questionnaire <- applyEnv serverConfig.questionnaire
    temporaryFile <- applyEnv serverConfig.temporaryFile
    userToken <- applyEnv serverConfig.userToken
    logging <- applyEnv serverConfig.logging
    cloud <- applyEnv serverConfig.cloud
    admin <- applyEnv serverConfig.admin
    modules <- applyEnv serverConfig.modules
    return ServerConfig {..}

instance FromEnv ServerConfigGeneral where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "GENERAL_ENVIRONMENT" c.environment (\x -> c {environment = x})
      , \c -> applyStringEnvVariable "GENERAL_CLIENT_URL" c.clientUrl (\x -> c {clientUrl = x} :: ServerConfigGeneral)
      , \c -> applyEnvVariable "GENERAL_SERVER_PORT" c.serverPort (\x -> c {serverPort = x})
      , \c -> applyStringEnvVariable "GENERAL_SECRET" c.secret (\x -> c {secret = x})
      , \c -> applyRSAPrivateKeyEnvVariable "GENERAL_RSA_PRIVATE_KEY" c.rsaPrivateKey (\x -> c {rsaPrivateKey = x} :: ServerConfigGeneral)
      , \c -> applyStringEnvVariable "GENERAL_INTEGRATION_CONFIG" c.integrationConfig (\x -> c {integrationConfig = x})
      ]

instance FromEnv ServerConfigJwt where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "JWT_EXPIRATION" c.expiration (\x -> c {expiration = x})
      ]

instance FromEnv ServerConfigRoles where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "ROLES_ADMIN" c.admin (\x -> c {admin = x} :: ServerConfigRoles)
      , \c -> applyEnvVariable "ROLES_DATA_STEWARD" c.dataSteward (\x -> c {dataSteward = x} :: ServerConfigRoles)
      , \c -> applyEnvVariable "ROLES_RESEARCHER" c.researcher (\x -> c {researcher = x} :: ServerConfigRoles)
      ]

instance FromEnv ServerConfigRegistry where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyStringEnvVariable "REGISTRY_URL" c.url (\x -> c {url = x} :: ServerConfigRegistry)
      , \c -> applyStringEnvVariable "REGISTRY_CLIENT_URL" c.clientUrl (\x -> c {clientUrl = x} :: ServerConfigRegistry)
      , \c -> applyEnvVariable "REGISTRY_SYNC_ENABLED" c.sync.enabled (\x -> c {sync = c.sync {enabled = x}} :: ServerConfigRegistry)
      , \c -> applyStringEnvVariable "REGISTRY_SYNC_CRON" c.sync.cron (\x -> c {sync = c.sync {cron = x}} :: ServerConfigRegistry)
      ]

instance FromEnv ServerConfigActionKey where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "ACTION_KEY_CLEAN_ENABLED" c.clean.enabled (\x -> c {clean = c.clean {enabled = x}} :: ServerConfigActionKey)
      , \c -> applyStringEnvVariable "ACTION_KEY_CLEAN_CRON" c.clean.cron (\x -> c {clean = c.clean {cron = x}} :: ServerConfigActionKey)
      ]

instance FromEnv ServerConfigBranch where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "BRANCH_SQUASH_ENABLED" c.squash.enabled (\x -> c {squash = c.squash {enabled = x}} :: ServerConfigBranch)
      , \c -> applyStringEnvVariable "BRANCH_SQUASH_CRON" c.squash.cron (\x -> c {squash = c.squash {cron = x}} :: ServerConfigBranch)
      ]

instance FromEnv ServerConfigCache where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "CACHE_DATA_EXPIRATION" c.dataExpiration (\x -> c {dataExpiration = x})
      , \c -> applyEnvVariable "CACHE_WEBSOCKET_EXPIRATION" c.websocketExpiration (\x -> c {websocketExpiration = x})
      , \c -> applyEnvVariable "CACHE_PURGE_EXPIRED_ENABLED" c.purgeExpired.enabled (\x -> c {purgeExpired = c.purgeExpired {enabled = x}} :: ServerConfigCache)
      , \c -> applyStringEnvVariable "CACHE_PURGE_EXPIRED_CRON" c.purgeExpired.cron (\x -> c {purgeExpired = c.purgeExpired {cron = x}} :: ServerConfigCache)
      ]

instance FromEnv ServerConfigDocument where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "DOCUMENT_CLEAN_ENABLED" c.clean.enabled (\x -> c {clean = c.clean {enabled = x}} :: ServerConfigDocument)
      , \c -> applyStringEnvVariable "DOCUMENT_CLEAN_CRON" c.clean.cron (\x -> c {clean = c.clean {cron = x}} :: ServerConfigDocument)
      ]

instance FromEnv ServerConfigFeedback where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyStringEnvVariable "FEEDBACK_API_URL" c.apiUrl (\x -> c {apiUrl = x} :: ServerConfigFeedback)
      , \c -> applyStringEnvVariable "FEEDBACK_WEB_URL" c.webUrl (\x -> c {webUrl = x} :: ServerConfigFeedback)
      , \c -> applyEnvVariable "FEEDBACK_SYNC_ENABLED" c.sync.enabled (\x -> c {sync = c.sync {enabled = x}} :: ServerConfigFeedback)
      , \c -> applyStringEnvVariable "FEEDBACK_SYNC_CRON" c.sync.cron (\x -> c {sync = c.sync {cron = x}} :: ServerConfigFeedback)
      ]

instance FromEnv ServerConfigPlan where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "PLAN_RECOMPUTE_JOB_ENABLED" c.recomputeJob.enabled (\x -> c {recomputeJob = c.recomputeJob {enabled = x}} :: ServerConfigPlan)
      , \c -> applyStringEnvVariable "PLAN_RECOMPUTE_JOB_CRON" c.recomputeJob.cron (\x -> c {recomputeJob = c.recomputeJob {cron = x}} :: ServerConfigPlan)
      ]

instance FromEnv ServerConfigQuestionnaire where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "QUESTIONNAIRE_CLEAN_ENABLED" c.clean.enabled (\x -> c {clean = c.clean {enabled = x}} :: ServerConfigQuestionnaire)
      , \c -> applyStringEnvVariable "QUESTIONNAIRE_CLEAN_CRON" c.clean.cron (\x -> c {clean = c.clean {cron = x}} :: ServerConfigQuestionnaire)
      , \c -> applyEnvVariable "QUESTIONNAIRE_RECOMPUTE_INDICATION_ENABLED" c.recomputeIndication.enabled (\x -> c {recomputeIndication = c.recomputeIndication {enabled = x}} :: ServerConfigQuestionnaire)
      , \c -> applyStringEnvVariable "QUESTIONNAIRE_RECOMPUTE_INDICATION_CRON" c.recomputeIndication.cron (\x -> c {recomputeIndication = c.recomputeIndication {cron = x}} :: ServerConfigQuestionnaire)
      , \c -> applyEnvVariable "QUESTIONNAIRE_SQUASH_ENABLED" c.squash.enabled (\x -> c {squash = c.squash {enabled = x}} :: ServerConfigQuestionnaire)
      , \c -> applyStringEnvVariable "QUESTIONNAIRE_SQUASH_CRON" c.squash.cron (\x -> c {squash = c.squash {cron = x}} :: ServerConfigQuestionnaire)
      ]

instance FromEnv ServerConfigTemporaryFile where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "TEMPORARY_FILE_CLEAN_ENABLED" c.clean.enabled (\x -> c {clean = c.clean {enabled = x}} :: ServerConfigTemporaryFile)
      , \c -> applyStringEnvVariable "TEMPORARY_FILE_CLEAN_CRON" c.clean.cron (\x -> c {clean = c.clean {cron = x}} :: ServerConfigTemporaryFile)
      ]

instance FromEnv ServerConfigUserToken where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "USER_TOKEN_CLEAN_ENABLED" c.clean.enabled (\x -> c {clean = c.clean {enabled = x}} :: ServerConfigUserToken)
      , \c -> applyStringEnvVariable "USER_TOKEN_CLEAN_CRON" c.clean.cron (\x -> c {clean = c.clean {cron = x}} :: ServerConfigUserToken)
      , \c -> applyEnvVariable "USER_TOKEN_EXPIRE_ENABLED" c.expire.enabled (\x -> c {expire = c.expire {enabled = x}} :: ServerConfigUserToken)
      , \c -> applyStringEnvVariable "USER_TOKEN_EXPIRE_CRON" c.expire.cron (\x -> c {expire = c.expire {cron = x}} :: ServerConfigUserToken)
      ]

instance FromEnv ServerConfigAdmin where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "ADMIN_ENABLED" c.enabled (\x -> c {enabled = x} :: ServerConfigAdmin)
      ]

instance FromEnv ServerConfigModules where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyStringEnvVariable "MODULES_WIZARD_TITLE" c.wizard.title (\x -> c {wizard = c.wizard {title = x}} :: ServerConfigModules)
      , \c -> applyStringEnvVariable "MODULES_WIZARD_DESCRIPTION" c.wizard.description (\x -> c {wizard = c.wizard {description = x}} :: ServerConfigModules)
      , \c -> applyStringEnvVariable "MODULES_WIZARD_ICON" c.wizard.icon (\x -> c {wizard = c.wizard {icon = x}} :: ServerConfigModules)
      , \c -> applyMaybeStringEnvVariable "MODULES_WIZARD_URL" c.wizard.url (\x -> c {wizard = c.wizard {url = x}} :: ServerConfigModules)
      , \c -> applyStringEnvVariable "MODULES_ADMIN_TITLE" c.admin.title (\x -> c {admin = c.admin {title = x}} :: ServerConfigModules)
      , \c -> applyStringEnvVariable "MODULES_ADMIN_DESCRIPTION" c.admin.description (\x -> c {admin = c.admin {description = x}} :: ServerConfigModules)
      , \c -> applyStringEnvVariable "MODULES_ADMIN_ICON" c.admin.icon (\x -> c {admin = c.admin {icon = x}} :: ServerConfigModules)
      , \c -> applyMaybeStringEnvVariable "MODULES_ADMIN_URL" c.admin.url (\x -> c {admin = c.admin {url = x}} :: ServerConfigModules)
      , \c -> applyStringEnvVariable "MODULES_GUIDE_TITLE" c.guide.title (\x -> c {guide = c.guide {title = x}} :: ServerConfigModules)
      , \c -> applyStringEnvVariable "MODULES_GUIDE_DESCRIPTION" c.guide.description (\x -> c {guide = c.guide {description = x}} :: ServerConfigModules)
      , \c -> applyStringEnvVariable "MODULES_GUIDE_ICON" c.guide.icon (\x -> c {guide = c.guide {icon = x}} :: ServerConfigModules)
      , \c -> applyMaybeStringEnvVariable "MODULES_GUIDE_URL" c.guide.url (\x -> c {guide = c.guide {url = x}} :: ServerConfigModules)
      ]
