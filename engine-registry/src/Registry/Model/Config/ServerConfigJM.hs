module Registry.Model.Config.ServerConfigJM where

import Control.Lens ((^.))
import Control.Monad
import Data.Aeson

import LensesConfig
import Registry.Model.Config.ServerConfig
import Registry.Model.Config.ServerConfigDM
import Shared.Model.Config.EnvironmentJM ()
import Shared.Model.Config.ServerConfigDM
import Shared.Model.Config.ServerConfigJM ()

instance FromJSON ServerConfig where
  parseJSON (Object o) = do
    _serverConfigGeneral <- o .: "general"
    _serverConfigDatabase <- o .:? "database" .!= defaultDatabase
    _serverConfigS3 <- o .:? "s3" .!= defaultS3
    _serverConfigMail <- o .:? "mail" .!= defaultMail
    _serverConfigAnalytics <- o .:? "analytics" .!= defaultAnalytics
    _serverConfigLogging <- o .:? "logging" .!= defaultLogging
    return ServerConfig {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigGeneral where
  parseJSON (Object o) = do
    _serverConfigGeneralEnvironment <- o .:? "environment" .!= (defaultGeneral ^. environment)
    _serverConfigGeneralClientUrl <- o .: "clientUrl"
    _serverConfigGeneralServerPort <- o .:? "serverPort" .!= (defaultGeneral ^. serverPort)
    _serverConfigGeneralTemplateFolder <- o .:? "templateFolder" .!= (defaultGeneral ^. templateFolder)
    _serverConfigGeneralRemoteLocalizationUrl <-
      o .:? "remoteLocalizationUrl" .!= (defaultGeneral ^. remoteLocalizationUrl)
    return ServerConfigGeneral {..}
  parseJSON _ = mzero
