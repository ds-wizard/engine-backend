module Wizard.Model.Config.ServerConfigIM where

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Config.ServerConfigIM
import Wizard.Model.Config.ServerConfig
import WizardLib.Public.Model.Config.ServerConfigIM ()

instance FromEnv ServerConfig where
  applyEnv serverConfig = do
    general <- applyEnv serverConfig.general
    database <- applyEnv serverConfig.database
    s3 <- applyEnv serverConfig.s3
    aws <- applyEnv serverConfig.aws
    sentry <- applyEnv serverConfig.sentry
    userEmailLink <- applyEnv serverConfig.userEmailLink
    cache <- applyEnv serverConfig.cache
    document <- applyEnv serverConfig.document
    externalLink <- applyEnv serverConfig.externalLink
    feedback <- applyEnv serverConfig.feedback
    knowledgeModelEditor <- applyEnv serverConfig.knowledgeModelEditor
    project <- applyEnv serverConfig.project
    temporaryFile <- applyEnv serverConfig.temporaryFile
    userToken <- applyEnv serverConfig.userToken
    analyticalMails <- applyEnv serverConfig.analyticalMails
    logging <- applyEnv serverConfig.logging
    cloud <- applyEnv serverConfig.cloud
    persistentCommand <- applyEnv serverConfig.persistentCommand
    signalBridge <- applyEnv serverConfig.signalBridge
    admin <- applyEnv serverConfig.admin
    registry <- applyEnv serverConfig.registry
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

instance FromEnv ServerConfigUserEmailLink where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "USER_EMAIL_LINK_CLEAN_ENABLED" c.clean.enabled (\x -> c {clean = c.clean {enabled = x}} :: ServerConfigUserEmailLink)
      , \c -> applyStringEnvVariable "USER_EMAIL_LINK_CLEAN_CRON" c.clean.cron (\x -> c {clean = c.clean {cron = x}} :: ServerConfigUserEmailLink)
      ]

instance FromEnv ServerConfigCache where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "CACHE_DATA_EXPIRATION" c.dataExpiration (\x -> c {dataExpiration = x})
      , \c -> applyEnvVariable "CACHE_WEBSOCKET_EXPIRATION" c.websocketExpiration (\x -> c {websocketExpiration = x})
      , \c -> applyEnvVariable "CACHE_PURGE_EXPIRED_ENABLED" c.purgeExpired.enabled (\x -> c {purgeExpired = c.purgeExpired {enabled = x}} :: ServerConfigCache)
      , \c -> applyStringEnvVariable "CACHE_PURGE_EXPIRED_CRON" c.purgeExpired.cron (\x -> c {purgeExpired = c.purgeExpired {cron = x}} :: ServerConfigCache)
      , \c -> applyEnvVariable "CACHE_DATA_ENABLED" c.dataEnabled (\x -> c {dataEnabled = x} :: ServerConfigCache)
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

instance FromEnv ServerConfigKnowledgeModelEditor where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "KNOWLEDGE_MODEL_EDITOR_SQUASH_ENABLED" c.squash.enabled (\x -> c {squash = c.squash {enabled = x}} :: ServerConfigKnowledgeModelEditor)
      , \c -> applyStringEnvVariable "KNOWLEDGE_MODEL_EDITOR_SQUASH_CRON" c.squash.cron (\x -> c {squash = c.squash {cron = x}} :: ServerConfigKnowledgeModelEditor)
      ]

instance FromEnv ServerConfigProject where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "PROJECT_CLEAN_ENABLED" c.clean.enabled (\x -> c {clean = c.clean {enabled = x}} :: ServerConfigProject)
      , \c -> applyStringEnvVariable "PROJECT_CLEAN_CRON" c.clean.cron (\x -> c {clean = c.clean {cron = x}} :: ServerConfigProject)
      , \c -> applyEnvVariable "PROJECT_SQUASH_ENABLED" c.squash.enabled (\x -> c {squash = c.squash {enabled = x}} :: ServerConfigProject)
      , \c -> applyStringEnvVariable "PROJECT_SQUASH_CRON" c.squash.cron (\x -> c {squash = c.squash {cron = x}} :: ServerConfigProject)
      , \c -> applyEnvVariable "PROJECT_ASSIGNEE_NOTIFICATION_ENABLED" c.assigneeNotification.enabled (\x -> c {assigneeNotification = c.assigneeNotification {enabled = x}} :: ServerConfigProject)
      , \c -> applyStringEnvVariable "PROJECT_ASSIGNEE_NOTIFICATION_CRON" c.assigneeNotification.cron (\x -> c {assigneeNotification = c.assigneeNotification {cron = x}} :: ServerConfigProject)
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

instance FromEnv ServerConfigSignalBridge where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "SIGNAL_BRIDGE_ENABLED" c.enabled (\x -> c {enabled = x} :: ServerConfigSignalBridge)
      , \c -> applyStringEnvVariable "SIGNAL_BRIDGE_UPDATE_PERMS_ARN" c.updatePermsArn (\x -> c {updatePermsArn = x} :: ServerConfigSignalBridge)
      , \c -> applyStringEnvVariable "SIGNAL_BRIDGE_UPDATE_USER_GROUP_ARN" c.updateUserGroupArn (\x -> c {updateUserGroupArn = x} :: ServerConfigSignalBridge)
      , \c -> applyStringEnvVariable "SIGNAL_BRIDGE_SET_PROJECT_ARN" c.setProjectArn (\x -> c {setProjectArn = x} :: ServerConfigSignalBridge)
      , \c -> applyStringEnvVariable "SIGNAL_BRIDGE_ADD_EVENT_ARN" c.addEventArn (\x -> c {addEventArn = x} :: ServerConfigSignalBridge)
      , \c -> applyStringEnvVariable "SIGNAL_BRIDGE_ADD_FILE_ARN" c.addFileArn (\x -> c {addFileArn = x} :: ServerConfigSignalBridge)
      , \c -> applyStringEnvVariable "SIGNAL_BRIDGE_LOG_OUT_ALL_ARN" c.logOutAllArn (\x -> c {logOutAllArn = x} :: ServerConfigSignalBridge)
      ]

instance FromEnv ServerConfigAdmin where
  applyEnv serverConfig =
    applyEnvVariables
      serverConfig
      [ \c -> applyEnvVariable "ADMIN_ENABLED" c.enabled (\x -> c {enabled = x} :: ServerConfigAdmin)
      , \c -> applyStringEnvVariable "ADMIN_SERVER_URL" c.serverUrl (\x -> c {serverUrl = x} :: ServerConfigAdmin)
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
