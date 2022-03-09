module Shared.Model.Config.ServerConfigJM where

import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Logger (LogLevel(..))
import Data.Aeson

import LensesConfig
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.ServerConfigDM

instance FromJSON ServerConfigDatabase where
  parseJSON (Object o) = do
    _serverConfigDatabaseConnectionString <- o .:? "connectionString" .!= (defaultDatabase ^. connectionString)
    _serverConfigDatabaseStripeSize <- o .:? "stripeSize" .!= (defaultDatabase ^. stripeSize)
    _serverConfigDatabaseConnectionTimeout <- o .:? "connectionTimeout" .!= (defaultDatabase ^. connectionTimeout)
    _serverConfigDatabaseMaxConnections <- o .:? "maxConnections" .!= (defaultDatabase ^. maxConnections)
    return ServerConfigDatabase {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigS3 where
  parseJSON (Object o) = do
    _serverConfigS3Url <- o .:? "url" .!= (defaultS3 ^. url)
    _serverConfigS3PublicUrl <- o .:? "publicUrl" .!= (defaultS3 ^. publicUrl)
    _serverConfigS3Username <- o .:? "username" .!= (defaultS3 ^. username)
    _serverConfigS3Password <- o .:? "password" .!= (defaultS3 ^. password)
    _serverConfigS3Bucket <- o .:? "bucket" .!= (defaultS3 ^. bucket)
    _serverConfigS3Region <- o .:? "region" .!= (defaultS3 ^. region)
    return ServerConfigS3 {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigAnalytics where
  parseJSON (Object o) = do
    _serverConfigAnalyticsEnabled <- o .:? "enabled" .!= (defaultAnalytics ^. enabled)
    _serverConfigAnalyticsEmail <- o .:? "email" .!= (defaultAnalytics ^. email)
    return ServerConfigAnalytics {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigSentry where
  parseJSON (Object o) = do
    _serverConfigSentryEnabled <- o .:? "enabled" .!= (defaultSentry ^. enabled)
    _serverConfigSentryDsn <- o .:? "dsn" .!= (defaultSentry ^. dsn)
    return ServerConfigSentry {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigLogging where
  parseJSON (Object o) = do
    _serverConfigLoggingLevel <- o .:? "level" .!= (defaultLogging ^. level)
    _serverConfigLoggingHttpClientDebug <- o .:? "httpClientDebug" .!= (defaultLogging ^. httpClientDebug)
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
    _serverConfigCloudEnabled <- o .: "enabled"
    _serverConfigCloudDomain <- o .:? "domain" .!= Nothing
    return ServerConfigCloud {..}
  parseJSON _ = mzero
