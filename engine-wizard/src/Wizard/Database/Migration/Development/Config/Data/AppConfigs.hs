module Wizard.Database.Migration.Development.Config.Data.AppConfigs where

import Data.Maybe (fromJust)
import Data.Time

import Wizard.Model.Common.SensitiveData
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.AppConfigEM ()
import Wizard.Model.Config.SimpleFeature

defaultSecret = "01234567890123456789012345678901"

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { _appConfigFeatures = defaultFeatures
    , _appConfigClient = defaultClient
    , _appConfigInfo = defaultInfo
    , _appConfigAffiliation = defaultAffiliation
    , _appConfigAuth = defaultAuth
    , _appConfigCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _appConfigUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

defaultAppConfigEncrypted :: AppConfig
defaultAppConfigEncrypted = process defaultSecret defaultAppConfig

defaultFeatures :: AppConfigFeatures
defaultFeatures =
  AppConfigFeatures
    { _appConfigFeaturesPublicQuestionnaire = SimpleFeature False
    , _appConfigFeaturesLevels = SimpleFeature True
    , _appConfigFeaturesQuestionnaireAccessibility = SimpleFeature True
    }

defaultClient :: AppConfigClient
defaultClient =
  AppConfigClient
    { _appConfigClientPrivacyUrl = "https://ds-wizard.org/privacy.html"
    , _appConfigClientAppTitle = Nothing
    , _appConfigClientAppTitleShort = Nothing
    , _appConfigClientSupportEmail = Nothing
    , _appConfigClientSupportRepositoryName = Nothing
    , _appConfigClientSupportRepositoryUrl = Nothing
    , _appConfigClientDashboard = Just defaultClientDashboard
    , _appConfigClientCustomMenuLinks = [defaultClientCustomLink]
    }

defaultClientDashboard :: AppConfigClientDashboard
defaultClientDashboard =
  AppConfigClientDashboard
    { _appConfigClientDashboardAdmin = ["Welcome"]
    , _appConfigClientDashboardDataSteward = ["Welcome"]
    , _appConfigClientDashboardResearcher = ["Welcome"]
    }

defaultClientCustomLink :: AppConfigClientCustomMenuLink
defaultClientCustomLink =
  AppConfigClientCustomMenuLink
    { _appConfigClientCustomMenuLinkIcon = "faq"
    , _appConfigClientCustomMenuLinkTitle = "My Link"
    , _appConfigClientCustomMenuLinkUrl = "http://example.prg"
    , _appConfigClientCustomMenuLinkNewWindow = False
    }

defaultInfo :: AppConfigInfo
defaultInfo =
  AppConfigInfo
    {_appConfigInfoWelcomeWarning = Nothing, _appConfigInfoWelcomeInfo = Nothing, _appConfigInfoLoginInfo = Nothing}

defaultAffiliation :: AppConfigAffiliation
defaultAffiliation = AppConfigAffiliation {_appConfigAffiliationAffiliations = []}

defaultAuth = AppConfigAuth {_appConfigAuthInternal = defaultAuthInternal, _appConfigAuthExternal = defaultAuthExternal}

defaultAuthInternal = AppConfigAuthInternal {_appConfigAuthInternalRegistration = SimpleFeature True}

defaultAuthExternal = AppConfigAuthExternal {_appConfigAuthExternalServices = [defaultAuthExternalService]}

defaultAuthExternalService =
  AppConfigAuthExternalService
    { _appConfigAuthExternalServiceAId = "google"
    , _appConfigAuthExternalServiceName = "Google"
    , _appConfigAuthExternalServiceUrl = "https://accounts.google.com"
    , _appConfigAuthExternalServiceClientId = "9381928r948-2i9uawjkn32ku89uafwa.apps.googleusercontent.com"
    , _appConfigAuthExternalServiceClientSecret = "ijijsd89f72ujknjksfawfawf"
    , _appConfigAuthExternalServiceParameters = [defaultAuthExternalServiceParameter]
    , _appConfigAuthExternalServiceStyle = defaultAuthExternalServiceStyle
    }

defaultAuthExternalServiceParameter =
  AppConfigAuthExternalServiceParameter
    {_appConfigAuthExternalServiceParameterName = "hd", _appConfigAuthExternalServiceParameterValue = "google.com"}

defaultAuthExternalServiceStyle =
  AppConfigAuthExternalServiceStyle
    { _appConfigAuthExternalServiceStyleIcon = "fa-google"
    , _appConfigAuthExternalServiceStyleBackground = "#000"
    , _appConfigAuthExternalServiceStyleColor = "#FFF"
    }

-- ------------------------------------------------------------
-- ------------------------------------------------------------
editedFeatures :: AppConfigFeatures
editedFeatures = defaultFeatures {_appConfigFeaturesLevels = SimpleFeature False}

editedClient :: AppConfigClient
editedClient = defaultClient {_appConfigClientPrivacyUrl = "https://ds-wizard.org/privacy.html/EDITED"}

editedInfo :: AppConfigInfo
editedInfo = defaultInfo {_appConfigInfoWelcomeInfo = Just "EDITED: Welcome Info"}

editedAffiliation :: AppConfigAffiliation
editedAffiliation = defaultAffiliation {_appConfigAffiliationAffiliations = ["https://myuniversity.org"]}

editedAuth :: AppConfigAuth
editedAuth =
  defaultAuth
    {_appConfigAuthInternal = AppConfigAuthInternal {_appConfigAuthInternalRegistration = SimpleFeature False}}
