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
      , \c -> applyStringEnvVariable "GENERAL_CLIENT_STYLE_BUILDER_URL" c.clientStyleBuilderUrl (\x -> c {clientStyleBuilderUrl = x})
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

instance FromEnv ServerConfigPersistentCommand where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "PERSISTENT_COMMAND_LISTENER_JOB_ENABLED" c.listenerJob.enabled (\x -> c {listenerJob = c.listenerJob {enabled = x}} :: ServerConfigPersistentCommand)
      , \c -> applyEnvVariable "PERSISTENT_COMMAND_RETRY_JOB_ENABLED" c.retryJob.enabled (\x -> c {retryJob = c.retryJob {enabled = x}} :: ServerConfigPersistentCommand)
      , \c -> applyStringEnvVariable "PERSISTENT_COMMAND_RETRY_JOB_CRON" c.retryJob.cron (\x -> c {retryJob = c.retryJob {cron = x}} :: ServerConfigPersistentCommand)
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
      ]

instance FromEnv ServerConfigLogging where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "LOGGING_LEVEL" c.level (\x -> c {level = x})
      , \c -> applyEnvVariable "LOGGING_HTTP_CLIENT_DEBUG" c.httpClientDebug (\x -> c {httpClientDebug = x})
      , \c -> applyEnvVariable "LOGGING_WEBSOCKET_DEBUG" c.websocketDebug (\x -> c {websocketDebug = x})
      ]

instance FromEnv ServerConfigCloud where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "CLOUD_ENABLED" c.enabled (\x -> c {enabled = x} :: ServerConfigCloud)
      , \c -> applyMaybeStringEnvVariable "CLOUD_DOMAIN" c.domain (\x -> c {domain = x})
      , \c -> applyEnvVariable "CLOUD_PUBLIC_REGISTRATION_ENABLED" c.publicRegistrationEnabled (\x -> c {publicRegistrationEnabled = x})
      ]

instance FromEnv ServerConfigAdmin where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "ADMIN_ENABLED" c.enabled (\x -> c {enabled = x} :: ServerConfigAdmin)
      , \c -> applyStringEnvVariable "ADMIN_URL" c.url (\x -> c {url = x} :: ServerConfigAdmin)
      , \c -> applyStringEnvVariable "ADMIN_TOKEN" c.token (\x -> c {token = x} :: ServerConfigAdmin)
      ]
