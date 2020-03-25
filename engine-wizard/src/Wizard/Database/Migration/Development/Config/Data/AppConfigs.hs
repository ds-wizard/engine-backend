module Wizard.Database.Migration.Development.Config.Data.AppConfigs where

import Data.Maybe (fromJust)
import Data.Time

import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.SimpleFeature

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { _appConfigFeatures = defaultFeatures
    , _appConfigClient = defaultClient
    , _appConfigInfo = defaultInfo
    , _appConfigAffiliation = defaultAffiliation
    , _appConfigCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _appConfigUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

defaultFeatures :: AppConfigFeatures
defaultFeatures =
  AppConfigFeatures
    { _appConfigFeaturesRegistration = SimpleFeature True
    , _appConfigFeaturesPublicQuestionnaire = SimpleFeature False
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

-- ------------------------------------------------------------
-- ------------------------------------------------------------
editedFeatures :: AppConfigFeatures
editedFeatures = defaultFeatures {_appConfigFeaturesRegistration = SimpleFeature False}

editedClient :: AppConfigClient
editedClient = defaultClient {_appConfigClientPrivacyUrl = "https://ds-wizard.org/privacy.html/EDITED"}

editedInfo :: AppConfigInfo
editedInfo = defaultInfo {_appConfigInfoWelcomeInfo = Just "EDITED: Welcome Info"}

editedAffiliation :: AppConfigAffiliation
editedAffiliation = defaultAffiliation {_appConfigAffiliationAffiliations = ["https://myuniversity.org"]}
