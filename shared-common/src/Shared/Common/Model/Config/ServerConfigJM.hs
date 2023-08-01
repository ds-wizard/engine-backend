module Shared.Common.Model.Config.ServerConfigJM where

import Control.Monad
import Control.Monad.Logger (LogLevel (..))
import Data.Aeson

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Model.Config.ServerConfigDM

instance FromJSON ServerConfigDatabase where
  parseJSON (Object o) = do
    connectionString <- o .:? "connectionString" .!= defaultDatabase.connectionString
    stripeSize <- o .:? "stripeSize" .!= defaultDatabase.stripeSize
    connectionTimeout <- o .:? "connectionTimeout" .!= defaultDatabase.connectionTimeout
    maxConnections <- o .:? "maxConnections" .!= defaultDatabase.maxConnections
    return ServerConfigDatabase {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigS3 where
  parseJSON (Object o) = do
    url <- o .:? "url" .!= defaultS3.url
    username <- o .:? "username" .!= defaultS3.username
    password <- o .:? "password" .!= defaultS3.password
    bucket <- o .:? "bucket" .!= defaultS3.bucket
    region <- o .:? "region" .!= defaultS3.region
    return ServerConfigS3 {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigAnalytics where
  parseJSON (Object o) = do
    enabled <- o .:? "enabled" .!= defaultAnalytics.enabled
    email <- o .:? "email" .!= defaultAnalytics.email
    return ServerConfigAnalytics {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigSentry where
  parseJSON (Object o) = do
    enabled <- o .:? "enabled" .!= defaultSentry.enabled
    dsn <- o .:? "dsn" .!= defaultSentry.dsn
    return ServerConfigSentry {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigLogging where
  parseJSON (Object o) = do
    level <- o .:? "level" .!= defaultLogging.level
    httpClientDebug <- o .:? "httpClientDebug" .!= defaultLogging.httpClientDebug
    websocketDebug <- o .:? "websocketDebug" .!= defaultLogging.websocketDebug
    return ServerConfigLogging {..}
  parseJSON _ = mzero

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel" parse
    where
      parse "DEBUG" = return LevelDebug
      parse "INFO" = return LevelInfo
      parse "WARN" = return LevelWarn
      parse "ERROR" = return LevelError
      parse _ = fail "Log Level has unsupported log level"

instance FromJSON ServerConfigCloud where
  parseJSON (Object o) = do
    enabled <- o .:? "enabled" .!= defaultCloud.enabled
    domain <- o .:? "domain" .!= defaultCloud.domain
    publicRegistrationEnabled <-
      o .:? "publicRegistrationEnabled" .!= defaultCloud.publicRegistrationEnabled
    prefix <- o .:? "prefix" .!= defaultCloud.prefix
    return ServerConfigCloud {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigCronWorker where
  parseJSON (Object o) = do
    enabled <- o .:? "enabled" .!= True
    cron <- o .: "cron"
    return ServerConfigCronWorker {..}
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
