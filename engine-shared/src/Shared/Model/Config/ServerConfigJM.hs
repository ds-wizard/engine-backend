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
    return ServerConfigS3 {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigMail where
  parseJSON (Object o) = do
    _serverConfigMailEnabled <- o .:? "enabled" .!= (defaultMail ^. enabled)
    _serverConfigMailName <- o .:? "name" .!= (defaultMail ^. name)
    _serverConfigMailEmail <- o .: "email" .!= (defaultMail ^. email)
    _serverConfigMailSsl <- o .:? "ssl" .!= (defaultMail ^. ssl)
    _serverConfigMailHost <- o .: "host" .!= (defaultMail ^. host)
    _serverConfigMailPort <-
      o .:? "port" .!=
      (if _serverConfigMailSsl
         then 465
         else 25)
    _serverConfigMailAuthEnabled <- o .:? "authEnabled" .!= (defaultMail ^. authEnabled)
    _serverConfigMailUsername <- o .:? "username" .!= (defaultMail ^. username)
    _serverConfigMailPassword <- o .:? "password" .!= (defaultMail ^. password)
    return ServerConfigMail {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigAnalytics where
  parseJSON (Object o) = do
    _serverConfigAnalyticsEnabled <- o .:? "enabled" .!= (defaultAnalytics ^. enabled)
    _serverConfigAnalyticsEmail <- o .:? "email" .!= (defaultAnalytics ^. email)
    return ServerConfigAnalytics {..}
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

instance FromJSON ServerConfigExperimental where
  parseJSON (Object o) = do
    _serverConfigExperimentalMoreAppsEnabled <- o .: "moreAppsEnabled"
    return ServerConfigExperimental {..}
  parseJSON _ = mzero
