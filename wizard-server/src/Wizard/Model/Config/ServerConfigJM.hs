module Wizard.Model.Config.ServerConfigJM where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.String (fromString)

import Shared.Common.Constant.DummyRsaPrivateKey
import Shared.Common.Localization.Messages.Internal
import Shared.Common.Model.Config.ServerConfigDM
import Shared.Common.Model.Config.ServerConfigJM ()
import Shared.Common.Util.Crypto
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfigDM
import Wizard.Model.User.User

instance FromJSON ServerConfig where
  parseJSON (Object o) = do
    general <- o .:? "general" .!= defaultGeneral
    database <- o .:? "database" .!= defaultDatabase
    s3 <- o .:? "s3" .!= defaultS3
    aws <- o .:? "aws" .!= defaultAws
    sentry <- o .:? "sentry" .!= defaultSentry
    jwt <- o .:? "jwt" .!= defaultJwt
    roles <- o .:? "roles" .!= defaultRoles
    actionKey <- o .:? "actionKey" .!= defaultActionKey
    branch <- o .:? "branch" .!= defaultBranch
    cache <- o .:? "cache" .!= defaultCache
    document <- o .:? "document" .!= defaultDocument
    feedback <- o .:? "feedback" .!= defaultFeedback
    questionnaire <- o .:? "questionnaire" .!= defaultQuestionnaire
    temporaryFile <- o .:? "temporaryFile" .!= defaultTemporaryFile
    userToken <- o .:? "userToken" .!= defaultUserToken
    analyticalMails <- o .:? "analyticalMails" .!= defaultAnalyticalMails
    logging <- o .:? "logging" .!= defaultLogging
    cloud <- o .:? "cloud" .!= defaultCloud
    plan <- o .:? "plan" .!= defaultPlan
    persistentCommand <- o .:? "persistentCommand" .!= defaultPersistentCommand
    signalBridge <- o .:? "signalBridge" .!= defaultSignalBridge
    admin <- o .:? "admin" .!= defaultAdmin
    registry <- o .:? "registry" .!= defaultRegistry
    modules <- o .:? "modules" .!= defaultModules
    return ServerConfig {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigGeneral where
  parseJSON (Object o) = do
    environment <- o .:? "environment" .!= defaultGeneral.environment
    clientUrl <- o .:? "clientUrl" .!= defaultGeneral.clientUrl
    serverPort <- o .:? "serverPort" .!= defaultGeneral.serverPort
    secret <- o .:? "secret" .!= defaultGeneral.secret
    mRsaPrivateKeyString <- o .:? "rsaPrivateKey"
    rsaPrivateKey <-
      case mRsaPrivateKeyString of
        Just rsaPrivateKeyString ->
          case readRSAPrivateKey . BS.pack $ rsaPrivateKeyString of
            Just privateKey -> return privateKey
            Nothing -> fail _ERROR_SERVICE_CONFIG__VALIDATION_CFG_RSA_PRIVATE_KEY_FORMAT
        Nothing -> return dummyRsaPrivateKey
    integrationConfig <- o .:? "integrationConfig" .!= defaultGeneral.integrationConfig
    return ServerConfigGeneral {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigRoles where
  parseJSON (Object o) = do
    admin <- o .:? fromString _USER_ROLE_ADMIN .!= defaultRoles.admin
    dataSteward <- o .:? fromString _USER_ROLE_DATA_STEWARD .!= defaultRoles.dataSteward
    researcher <- o .:? fromString _USER_ROLE_RESEARCHER .!= defaultRoles.researcher
    return ServerConfigRoles {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigActionKey where
  parseJSON (Object o) = do
    clean <- o .:? "clean" .!= defaultActionKey.clean
    return ServerConfigActionKey {..}
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

instance FromJSON ServerConfigQuestionnaire where
  parseJSON (Object o) = do
    clean <- o .:? "clean" .!= defaultQuestionnaire.clean
    squash <- o .:? "squash" .!= defaultQuestionnaire.squash
    assigneeNotification <- o .:? "assigneeNotification" .!= defaultQuestionnaire.assigneeNotification
    return ServerConfigQuestionnaire {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigTemporaryFile where
  parseJSON (Object o) = do
    clean <- o .:? "clean" .!= defaultTemporaryFile.clean
    return ServerConfigTemporaryFile {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigUserToken where
  parseJSON (Object o) = do
    clean <- o .:? "clean" .!= defaultUserToken.clean
    expire <- o .:? "expire" .!= defaultUserToken.expire
    return ServerConfigUserToken {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigSignalBridge where
  parseJSON (Object o) = do
    enabled <- o .:? "enabled" .!= defaultSignalBridge.enabled
    updatePermsArn <- o .:? "updatePermsArn" .!= defaultSignalBridge.updatePermsArn
    updateUserGroupArn <- o .:? "updateUserGroupArn" .!= defaultSignalBridge.updateUserGroupArn
    setQuestionnaireArn <- o .:? "setQuestionnaireArn" .!= defaultSignalBridge.setQuestionnaireArn
    addFileArn <- o .:? "addFileArn" .!= defaultSignalBridge.addFileArn
    logOutAllArn <- o .:? "logOutAllArn" .!= defaultSignalBridge.logOutAllArn
    return ServerConfigSignalBridge {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigAdmin where
  parseJSON (Object o) = do
    enabled <- o .:? "enabled" .!= defaultAdmin.enabled
    return ServerConfigAdmin {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigRegistry where
  parseJSON (Object o) = do
    url <- o .:? "url" .!= defaultRegistry.url
    clientUrl <- o .:? "clientUrl" .!= defaultRegistry.clientUrl
    sync <- o .:? "sync" .!= defaultRegistry.sync
    return ServerConfigRegistry {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigModules where
  parseJSON (Object o) = do
    wizard <- o .:? "wizard" .!= defaultModules.wizard
    admin <- o .:? "admin" .!= defaultModules.admin
    integrationHub <- o .:? "integrationHub" .!= defaultModules.integrationHub
    analytics <- o .:? "analytics" .!= defaultModules.analytics
    guide <- o .:? "guide" .!= defaultModules.guide
    return ServerConfigModules {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigModule where
  parseJSON (Object o) = do
    title <- o .:? "title" .!= defaultModule.title
    description <- o .:? "description" .!= defaultModule.description
    icon <- o .:? "icon" .!= defaultModule.icon
    url <- o .:? "url" .!= defaultModule.url
    return ServerConfigModule {..}
  parseJSON _ = mzero
