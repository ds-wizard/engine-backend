module Wizard.Model.Config.ServerConfigJM where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as BS

import Shared.Common.Constant.DummyRsaPrivateKey
import Shared.Common.Localization.Messages.Internal
import Shared.Common.Model.Config.ServerConfigDM
import Shared.Common.Model.Config.ServerConfigJM ()
import Shared.Common.Util.Crypto
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfigDM
import WizardLib.Public.Model.Config.ServerConfigDM
import WizardLib.Public.Model.Config.ServerConfigJM ()

instance FromJSON ServerConfig where
  parseJSON (Object o) = do
    general <- o .:? "general" .!= defaultGeneral
    database <- o .:? "database" .!= defaultDatabase
    s3 <- o .:? "s3" .!= defaultS3
    aws <- o .:? "aws" .!= defaultAws
    sentry <- o .:? "sentry" .!= defaultSentry
    userEmailLink <- o .:? "userEmailLink" .!= defaultUserEmailLink
    knowledgeModelEditor <- o .:? "knowledgeModelEditor" .!= defaultKnowledgeModelEditor
    cache <- o .:? "cache" .!= defaultCache
    document <- o .:? "document" .!= defaultDocument
    externalLink <- o .:? "externalLink" .!= defaultExternalLink
    feedback <- o .:? "feedback" .!= defaultFeedback
    project <- o .:? "project" .!= defaultProject
    temporaryFile <- o .:? "temporaryFile" .!= defaultTemporaryFile
    userToken <- o .:? "userToken" .!= defaultUserToken
    analyticalMails <- o .:? "analyticalMails" .!= defaultAnalyticalMails
    logging <- o .:? "logging" .!= defaultLogging
    cloud <- o .:? "cloud" .!= defaultCloud
    persistentCommand <- o .:? "persistentCommand" .!= defaultPersistentCommand
    signalBridge <- o .:? "signalBridge" .!= defaultSignalBridge
    admin <- o .:? "admin" .!= defaultAdmin
    registry <- o .:? "registry" .!= defaultRegistry
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

instance FromJSON ServerConfigUserEmailLink where
  parseJSON (Object o) = do
    clean <- o .:? "clean" .!= defaultUserEmailLink.clean
    return ServerConfigUserEmailLink {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigKnowledgeModelEditor where
  parseJSON (Object o) = do
    squash <- o .:? "squash" .!= defaultKnowledgeModelEditor.squash
    return ServerConfigKnowledgeModelEditor {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigCache where
  parseJSON (Object o) = do
    dataExpiration <- o .:? "dataExpiration" .!= defaultCache.dataExpiration
    websocketExpiration <- o .:? "websocketExpiration" .!= defaultCache.websocketExpiration
    purgeExpired <- o .:? "purgeExpired" .!= defaultCache.purgeExpired
    dataEnabled <- o .:? "dataEnabled" .!= defaultCache.dataEnabled
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

instance FromJSON ServerConfigProject where
  parseJSON (Object o) = do
    clean <- o .:? "clean" .!= defaultProject.clean
    squash <- o .:? "squash" .!= defaultProject.squash
    assigneeNotification <- o .:? "assigneeNotification" .!= defaultProject.assigneeNotification
    return ServerConfigProject {..}
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
    setProjectArn <- o .:? "setProjectArn" .!= defaultSignalBridge.setProjectArn
    addEventArn <- o .:? "addEventArn" .!= defaultSignalBridge.addEventArn
    addFileArn <- o .:? "addFileArn" .!= defaultSignalBridge.addFileArn
    logOutAllArn <- o .:? "logOutAllArn" .!= defaultSignalBridge.logOutAllArn
    return ServerConfigSignalBridge {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigAdmin where
  parseJSON (Object o) = do
    enabled <- o .:? "enabled" .!= defaultAdmin.enabled
    serverUrl <- o .:? "serverUrl" .!= defaultAdmin.serverUrl
    return ServerConfigAdmin {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigRegistry where
  parseJSON (Object o) = do
    url <- o .:? "url" .!= defaultRegistry.url
    clientUrl <- o .:? "clientUrl" .!= defaultRegistry.clientUrl
    sync <- o .:? "sync" .!= defaultRegistry.sync
    return ServerConfigRegistry {..}
  parseJSON _ = mzero
