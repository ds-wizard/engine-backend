module Registry.Model.Config.ServerConfigJM where

import Control.Monad
import Data.Aeson

import Registry.Model.Config.ServerConfig
import Registry.Model.Config.ServerConfigDM
import Shared.Model.Config.EnvironmentJM ()
import Shared.Model.Config.ServerConfigDM
import Shared.Model.Config.ServerConfigJM ()

instance FromJSON ServerConfig where
  parseJSON (Object o) = do
    general <- o .: "general"
    database <- o .:? "database" .!= defaultDatabase
    s3 <- o .:? "s3" .!= defaultS3
    analytics <- o .:? "analytics" .!= defaultAnalytics
    sentry <- o .:? "sentry" .!= defaultSentry
    logging <- o .:? "logging" .!= defaultLogging
    cloud <- o .:? "cloud" .!= defaultCloud
    return ServerConfig {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigGeneral where
  parseJSON (Object o) = do
    environment <- o .:? "environment" .!= defaultGeneral.environment
    clientUrl <- o .: "clientUrl"
    serverPort <- o .:? "serverPort" .!= defaultGeneral.serverPort
    remoteLocalizationUrl <- o .:? "remoteLocalizationUrl" .!= defaultGeneral.remoteLocalizationUrl
    return ServerConfigGeneral {..}
  parseJSON _ = mzero
