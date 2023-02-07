module Wizard.Model.Config.ServerConfigJM where

import Control.Monad
import Data.Aeson
import Data.String (fromString)

import Shared.Model.Config.EnvironmentJM ()
import Shared.Model.Config.ServerConfigDM
import Shared.Model.Config.ServerConfigJM ()
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfigDM
import Wizard.Model.User.User

instance FromJSON ServerConfig where
  parseJSON (Object o) = do
    general <- o .:? "general" .!= defaultGeneral
    database <- o .:? "database" .!= defaultDatabase
    s3 <- o .:? "s3" .!= defaultS3
    jwt <- o .:? "jwt" .!= defaultJwt
    roles <- o .:? "roles" .!= defaultRoles
    registry <- o .:? "registry" .!= defaultRegistry
    analytics <- o .:? "analytics" .!= defaultAnalytics
    sentry <- o .:? "sentry" .!= defaultSentry
    branch <- o .:? "branch" .!= defaultBranch
    cache <- o .:? "cache" .!= defaultCache
    document <- o .:? "document" .!= defaultDocument
    feedback <- o .:? "feedback" .!= defaultFeedback
    persistentCommand <- o .:? "persistentCommand" .!= defaultPersistentCommand
    plan <- o .:? "plan" .!= defaultPlan
    questionnaire <- o .:? "questionnaire" .!= defaultQuestionnaire
    userToken <- o .:? "userToken" .!= defaultUserToken
    logging <- o .:? "logging" .!= defaultLogging
    cloud <- o .:? "cloud" .!= defaultCloud
    return ServerConfig {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigGeneral where
  parseJSON (Object o) = do
    environment <- o .:? "environment" .!= defaultGeneral.environment
    clientUrl <- o .:? "clientUrl" .!= defaultGeneral.clientUrl
    serverPort <- o .:? "serverPort" .!= defaultGeneral.serverPort
    secret <- o .:? "secret" .!= defaultGeneral.secret
    integrationConfig <- o .:? "integrationConfig" .!= defaultGeneral.integrationConfig
    clientStyleBuilderUrl <- o .:? "clientStyleBuilderUrl" .!= defaultGeneral.clientStyleBuilderUrl
    return ServerConfigGeneral {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigJwt where
  parseJSON (Object o) = do
    version <- o .:? "version" .!= defaultJwt.version
    expiration <- o .:? "expiration" .!= defaultJwt.expiration
    return ServerConfigJwt {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigRoles where
  parseJSON (Object o) = do
    admin <- o .:? fromString _USER_ROLE_ADMIN .!= defaultRoles.admin
    dataSteward <- o .:? fromString _USER_ROLE_DATA_STEWARD .!= defaultRoles.dataSteward
    researcher <- o .:? fromString _USER_ROLE_RESEARCHER .!= defaultRoles.researcher
    return ServerConfigRoles {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigRegistry where
  parseJSON (Object o) = do
    url <- o .:? "url" .!= defaultRegistry.url
    clientUrl <- o .:? "clientUrl" .!= defaultRegistry.clientUrl
    sync <- o .:? "sync" .!= defaultRegistry.sync
    return ServerConfigRegistry {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigBranch where
  parseJSON (Object o) = do
    squash <- o .:? "squash" .!= defaultBranch.squash
    return ServerConfigBranch {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigCache where
  parseJSON (Object o) = do
    dataExpiration <- o .:? "dataExpiration" .!= defaultCache.dataExpiration
    websocketExpiration <- o .:? "websocketExpiration" .!= defaultCache.websocketExpiration
    purgeExpired <- o .:? "purgeExpired" .!= defaultCache.purgeExpired
    return ServerConfigCache {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigDocument where
  parseJSON (Object o) = do
    clean <- o .:? "clean" .!= defaultDocument.clean
    return ServerConfigDocument {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigFeedback where
  parseJSON (Object o) = do
    apiUrl <- o .:? "apiUrl" .!= defaultFeedback.apiUrl
    webUrl <- o .:? "webUrl" .!= defaultFeedback.webUrl
    sync <- o .:? "sync" .!= defaultFeedback.sync
    return ServerConfigFeedback {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigPersistentCommand where
  parseJSON (Object o) = do
    listenerJob <- o .:? "listenerJob" .!= defaultPersistentCommand.listenerJob
    retryJob <- o .:? "retryJob" .!= defaultPersistentCommand.retryJob
    return ServerConfigPersistentCommand {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigPersistentCommandListenerJob where
  parseJSON (Object o) = do
    enabled <- o .:? "enabled" .!= defaultPersistentCommandListenerJob.enabled
    return ServerConfigPersistentCommandListenerJob {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigPlan where
  parseJSON (Object o) = do
    recomputeJob <- o .:? "recomputeJob" .!= defaultPlan.recomputeJob
    return ServerConfigPlan {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigQuestionnaire where
  parseJSON (Object o) = do
    clean <- o .:? "clean" .!= defaultQuestionnaire.clean
    recomputeIndication <- o .:? "recomputeIndication" .!= defaultQuestionnaire.recomputeIndication
    squash <- o .:? "squash" .!= defaultQuestionnaire.squash
    return ServerConfigQuestionnaire {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigUserToken where
  parseJSON (Object o) = do
    clean <- o .:? "clean" .!= defaultUserToken.clean
    return ServerConfigUserToken {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigCronWorker where
  parseJSON (Object o) = do
    enabled <- o .:? "enabled" .!= True
    cron <- o .: "cron"
    return ServerConfigCronWorker {..}
  parseJSON _ = mzero
