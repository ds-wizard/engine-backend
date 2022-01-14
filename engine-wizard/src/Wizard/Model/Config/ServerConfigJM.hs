module Wizard.Model.Config.ServerConfigJM where

import Control.Lens ((^.))
import Control.Monad
import Data.Aeson
import qualified Data.Text as T

import LensesConfig
import Shared.Model.Config.EnvironmentJM ()
import Shared.Model.Config.ServerConfigDM
import Shared.Model.Config.ServerConfigJM ()
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Config.ServerConfigDM
import Wizard.Model.User.User

instance FromJSON ServerConfig where
  parseJSON (Object o) = do
    _serverConfigGeneral <- o .: "general"
    _serverConfigDatabase <- o .:? "database" .!= defaultDatabase
    _serverConfigS3 <- o .:? "s3" .!= defaultS3
    _serverConfigMessaging <- o .:? "messaging" .!= defaultMessaging
    _serverConfigJwt <- o .:? "jwt" .!= defaultJwt
    _serverConfigRoles <- o .:? "roles" .!= defaultRoles
    _serverConfigMail <- o .:? "mail" .!= defaultMail
    _serverConfigRegistry <- o .:? "registry" .!= defaultRegistry
    _serverConfigAnalytics <- o .:? "analytics" .!= defaultAnalytics
    _serverConfigBranch <- o .:? "branch" .!= defaultBranch
    _serverConfigDocument <- o .:? "document" .!= defaultDocument
    _serverConfigFeedback <- o .:? "feedback" .!= defaultFeedback
    _serverConfigQuestionnaire <- o .:? "questionnaire" .!= defaultQuestionnaire
    _serverConfigLogging <- o .:? "logging" .!= defaultLogging
    _serverConfigExperimental <- o .:? "experimental" .!= defaultExperimental
    return ServerConfig {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigGeneral where
  parseJSON (Object o) = do
    _serverConfigGeneralEnvironment <- o .:? "environment" .!= (defaultGeneral ^. environment)
    _serverConfigGeneralClientUrl <- o .: "clientUrl"
    _serverConfigGeneralServerPort <- o .:? "serverPort" .!= (defaultGeneral ^. serverPort)
    _serverConfigGeneralServiceToken <- o .: "serviceToken"
    _serverConfigGeneralSecret <- o .: "secret"
    _serverConfigGeneralIntegrationConfig <- o .:? "integrationConfig" .!= (defaultGeneral ^. integrationConfig)
    _serverConfigGeneralTemplateFolder <- o .:? "templateFolder" .!= (defaultGeneral ^. templateFolder)
    _serverConfigGeneralRemoteLocalizationUrl <-
      o .:? "remoteLocalizationUrl" .!= (defaultGeneral ^. remoteLocalizationUrl)
    _serverConfigGeneralClientStyleBuilderUrl <-
      o .:? "clientStyleBuilderUrl" .!= (defaultGeneral ^. clientStyleBuilderUrl)
    return ServerConfigGeneral {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigMessaging where
  parseJSON (Object o) = do
    _serverConfigMessagingEnabled <- o .:? "enabled" .!= (defaultMessaging ^. enabled)
    _serverConfigMessagingHost <- o .:? "host" .!= (defaultMessaging ^. host)
    _serverConfigMessagingPort <- o .:? "port" .!= (defaultMessaging ^. port)
    _serverConfigMessagingUsername <- o .:? "username" .!= (defaultMessaging ^. username)
    _serverConfigMessagingPassword <- o .:? "password" .!= (defaultMessaging ^. password)
    _serverConfigMessagingVhost <- o .:? "vhost" .!= (defaultMessaging ^. vhost)
    return ServerConfigMessaging {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigJwt where
  parseJSON (Object o) = do
    _serverConfigJwtVersion <- o .:? "version" .!= (defaultJwt ^. version)
    _serverConfigJwtExpiration <- o .:? "expiration" .!= (defaultJwt ^. expiration)
    return ServerConfigJwt {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigRoles where
  parseJSON (Object o) = do
    _serverConfigRolesAdmin <- o .:? T.pack _USER_ROLE_ADMIN .!= (defaultRoles ^. admin)
    _serverConfigRolesDataSteward <- o .:? T.pack _USER_ROLE_DATA_STEWARD .!= (defaultRoles ^. dataSteward)
    _serverConfigRolesResearcher <- o .:? T.pack _USER_ROLE_RESEARCHER .!= (defaultRoles ^. researcher)
    return ServerConfigRoles {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigRegistry where
  parseJSON (Object o) = do
    _serverConfigRegistryUrl <- o .:? "url" .!= (defaultRegistry ^. url)
    _serverConfigRegistryClientUrl <- o .:? "clientUrl" .!= (defaultRegistry ^. clientUrl)
    return ServerConfigRegistry {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigBranch where
  parseJSON (Object o) = do
    _serverConfigBranchSquash <- o .:? "squash" .!= (defaultBranch ^. squash)
    return ServerConfigBranch {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigDocument where
  parseJSON (Object o) = do
    _serverConfigDocumentClean <- o .:? "clean" .!= (defaultDocument ^. clean)
    return ServerConfigDocument {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigFeedback where
  parseJSON (Object o) = do
    _serverConfigFeedbackApiUrl <- o .:? "apiUrl" .!= (defaultFeedback ^. apiUrl)
    _serverConfigFeedbackWebUrl <- o .:? "webUrl" .!= (defaultFeedback ^. webUrl)
    _serverConfigFeedbackSync <- o .:? "sync" .!= (defaultFeedback ^. sync)
    return ServerConfigFeedback {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigQuestionnaire where
  parseJSON (Object o) = do
    _serverConfigQuestionnaireClean <- o .:? "clean" .!= (defaultQuestionnaire ^. clean)
    _serverConfigQuestionnaireSquash <- o .:? "squash" .!= (defaultQuestionnaire ^. squash)
    return ServerConfigQuestionnaire {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigCronWorker where
  parseJSON (Object o) = do
    _serverConfigCronWorkerEnabled <- o .: "enabled"
    _serverConfigCronWorkerCron <- o .: "cron"
    return ServerConfigCronWorker {..}
  parseJSON _ = mzero
