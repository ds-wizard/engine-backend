module Model.Config.AppConfigJM where

import Control.Lens ((^.))
import Control.Monad
import Data.Aeson

import LensesConfig
import Model.Config.AppConfig
import Model.Config.AppConfigDM
import Model.Config.EnvironmentJM ()

instance FromJSON AppConfig where
  parseJSON (Object o) = do
    _appConfigGeneral <- o .: "general"
    _appConfigClient <- o .:? "client" .!= defaultClient
    _appConfigDatabase <- o .:? "database" .!= defaultDatabase
    _appConfigMessaging <- o .:? "messaging" .!= defaultMessaging
    _appConfigJwt <- o .: "jwt"
    _appConfigRoles <- o .:? "roles" .!= defaultRoles
    _appConfigMail <- o .:? "mail" .!= defaultMail
    _appConfigRegistry <- o .:? "registry" .!= defaultRegistry
    _appConfigAnalytics <- o .:? "analytics" .!= defaultAnalytics
    _appConfigFeedback <- o .:? "feedback" .!= defaultFeedback
    return AppConfig {..}
  parseJSON _ = mzero

instance FromJSON AppConfigGeneral where
  parseJSON (Object o) = do
    _appConfigGeneralEnvironment <- o .:? "environment" .!= (defaultGeneral ^. environment)
    _appConfigGeneralClientUrl <- o .: "clientUrl"
    _appConfigGeneralServerPort <- o .:? "serverPort" .!= (defaultGeneral ^. serverPort)
    _appConfigGeneralServiceToken <- o .: "serviceToken"
    _appConfigGeneralIntegrationConfig <- o .:? "integrationConfig" .!= (defaultGeneral ^. integrationConfig)
    _appConfigGeneralRegistrationEnabled <- o .:? "registrationEnabled" .!= (defaultGeneral ^. registrationEnabled)
    _appConfigGeneralPublicQuestionnaireEnabled <-
      o .:? "publicQuestionnaireEnabled" .!= (defaultGeneral ^. publicQuestionnaireEnabled)
    _appConfigGeneralLevelsEnabled <- o .:? "levelsEnabled" .!= (defaultGeneral ^. levelsEnabled)
    _appConfigGeneralItemTitleEnabled <- o .:? "itemTitleEnabled" .!= (defaultGeneral ^. itemTitleEnabled)
    _appConfigGeneralQuestionnaireAccessibilityEnabled <-
      o .:? "questionnaireAccessibilityEnabled" .!= (defaultGeneral ^. questionnaireAccessibilityEnabled)
    return AppConfigGeneral {..}
  parseJSON _ = mzero

instance FromJSON AppConfigClient where
  parseJSON (Object o) = do
    _appConfigClientPrivacyUrl <- o .:? "privacyUrl" .!= (defaultClient ^. privacyUrl)
    _appConfigClientAppTitle <- o .:? "appTitle" .!= (defaultClient ^. appTitle)
    _appConfigClientAppTitleShort <- o .:? "appTitleShort" .!= (defaultClient ^. appTitleShort)
    _appConfigClientWelcomeWarning <- o .:? "welcomeWarning" .!= (defaultClient ^. welcomeWarning)
    _appConfigClientWelcomeInfo <- o .:? "welcomeInfo" .!= (defaultClient ^. welcomeInfo)
    _appConfigClientDashboard <- o .:? "dashboard" .!= (defaultClient ^. dashboard)
    _appConfigClientCustomMenuLinks <- o .:? "customMenuLinks" .!= (defaultClient ^. customMenuLinks)
    return AppConfigClient {..}
  parseJSON _ = mzero

instance FromJSON AppConfigClientDashboard where
  parseJSON (Object o) = do
    _appConfigClientDashboardAdmin <- o .:? "admin" .!= (defaultClientDashboard ^. admin)
    _appConfigClientDashboardDataSteward <- o .:? "dataSteward" .!= (defaultClientDashboard ^. dataSteward)
    _appConfigClientDashboardResearcher <- o .:? "researcher" .!= (defaultClientDashboard ^. researcher)
    return AppConfigClientDashboard {..}
  parseJSON _ = mzero

instance FromJSON AppConfigClientCustomMenuLink where
  parseJSON (Object o) = do
    _appConfigClientCustomMenuLinkIcon <- o .: "icon"
    _appConfigClientCustomMenuLinkTitle <- o .: "title"
    _appConfigClientCustomMenuLinkUrl <- o .: "url"
    _appConfigClientCustomMenuLinkNewWindow <- o .: "newWindow"
    return AppConfigClientCustomMenuLink {..}
  parseJSON _ = mzero

instance FromJSON AppConfigDatabase where
  parseJSON (Object o) = do
    _appConfigDatabaseHost <- o .:? "host" .!= (defaultDatabase ^. host)
    _appConfigDatabaseDatabaseName <- o .:? "databaseName" .!= (defaultDatabase ^. databaseName)
    _appConfigDatabasePort <- o .:? "port" .!= (defaultDatabase ^. port)
    _appConfigDatabaseAuthEnabled <- o .:? "authEnabled" .!= (defaultDatabase ^. authEnabled)
    _appConfigDatabaseUsername <- o .:? "username" .!= (defaultDatabase ^. username)
    _appConfigDatabasePassword <- o .:? "password" .!= (defaultDatabase ^. password)
    return AppConfigDatabase {..}
  parseJSON _ = mzero

instance FromJSON AppConfigMessaging where
  parseJSON (Object o) = do
    _appConfigMessagingEnabled <- o .:? "enabled" .!= (defaultMessaging ^. enabled)
    _appConfigMessagingHost <- o .:? "host" .!= (defaultMessaging ^. host)
    _appConfigMessagingPort <- o .:? "port" .!= (defaultMessaging ^. port)
    _appConfigMessagingUsername <- o .:? "username" .!= (defaultMessaging ^. username)
    _appConfigMessagingPassword <- o .:? "password" .!= (defaultMessaging ^. password)
    return AppConfigMessaging {..}
  parseJSON _ = mzero

instance FromJSON AppConfigJwt where
  parseJSON (Object o) = do
    _appConfigJwtSecret <- o .: "secret"
    _appConfigJwtVersion <- o .:? "version" .!= (defaultJwt ^. version)
    _appConfigJwtExpiration <- o .:? "expiration" .!= (defaultJwt ^. expiration)
    return AppConfigJwt {..}
  parseJSON _ = mzero

instance FromJSON AppConfigRoles where
  parseJSON (Object o) = do
    _appConfigRolesDefaultRole <- o .:? "defaultRole" .!= (defaultRoles ^. defaultRole)
    _appConfigRolesAdmin <- o .:? "admin" .!= (defaultRoles ^. admin)
    _appConfigRolesDataSteward <- o .:? "dataSteward" .!= (defaultRoles ^. dataSteward)
    _appConfigRolesResearcher <- o .:? "researcher" .!= (defaultRoles ^. researcher)
    return AppConfigRoles {..}
  parseJSON _ = mzero

instance FromJSON AppConfigMail where
  parseJSON (Object o) = do
    _appConfigMailEnabled <- o .:? "enabled" .!= (defaultMail ^. enabled)
    _appConfigMailName <- o .:? "name" .!= (defaultMail ^. name)
    _appConfigMailEmail <- o .: "email" .!= (defaultMail ^. email)
    _appConfigMailHost <- o .: "host" .!= (defaultMail ^. host)
    _appConfigMailPort <- o .:? "port" .!= (defaultMail ^. port)
    _appConfigMailSsl <- o .:? "ssl" .!= (defaultMail ^. ssl)
    _appConfigMailAuthEnabled <- o .:? "authEnabled" .!= (defaultMail ^. authEnabled)
    _appConfigMailUsername <- o .:? "username" .!= (defaultMail ^. username)
    _appConfigMailPassword <- o .:? "password" .!= (defaultMail ^. password)
    return AppConfigMail {..}
  parseJSON _ = mzero

instance FromJSON AppConfigRegistry where
  parseJSON (Object o) = do
    _appConfigRegistryEnabled <- o .:? "enabled" .!= (defaultRegistry ^. enabled)
    _appConfigRegistryUrl <- o .:? "url" .!= (defaultRegistry ^. url)
    _appConfigRegistryToken <- o .:? "token" .!= (defaultRegistry ^. token)
    _appConfigRegistryClientUrl <- o .:? "clientUrl" .!= (defaultRegistry ^. clientUrl)
    return AppConfigRegistry {..}
  parseJSON _ = mzero

instance FromJSON AppConfigAnalytics where
  parseJSON (Object o) = do
    _appConfigAnalyticsEnabled <- o .:? "enabled" .!= (defaultAnalytics ^. enabled)
    _appConfigAnalyticsEmail <- o .:? "email" .!= (defaultAnalytics ^. email)
    return AppConfigAnalytics {..}
  parseJSON _ = mzero

instance FromJSON AppConfigFeedback where
  parseJSON (Object o) = do
    _appConfigFeedbackEnabled <- o .:? "enabled" .!= (defaultFeedback ^. enabled)
    _appConfigFeedbackToken <- o .:? "token" .!= (defaultFeedback ^. token)
    _appConfigFeedbackOwner <- o .:? "owner" .!= (defaultFeedback ^. owner)
    _appConfigFeedbackRepo <- o .:? "repo" .!= (defaultFeedback ^. repo)
    _appConfigFeedbackIssueUrl <- o .:? "issueUrl" .!= (defaultFeedback ^. issueUrl)
    return AppConfigFeedback {..}
  parseJSON _ = mzero
