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
    , _appConfigClientWelcomeWarning = Nothing
    , _appConfigClientWelcomeInfo = Nothing
    , _appConfigClientLoginInfo = Nothing
    , _appConfigClientSupportEmail = Nothing
    , _appConfigClientSupportRepositoryName = Nothing
    , _appConfigClientSupportRepositoryUrl = Nothing
    , _appConfigClientDashboard = Just defaultClientDashboard
    , _appConfigClientCustomMenuLinks = []
    }

defaultClientDashboard :: AppConfigClientDashboard
defaultClientDashboard =
  AppConfigClientDashboard
    { _appConfigClientDashboardAdmin = ["Welcome"]
    , _appConfigClientDashboardDataSteward = ["Welcome"]
    , _appConfigClientDashboardResearcher = ["Welcome"]
    }

-- ------------------------------------------------------------
-- ------------------------------------------------------------
editedAppConfig :: AppConfig
editedAppConfig = defaultAppConfig {_appConfigFeatures = editedFeatures, _appConfigClient = editedClient}

editedFeatures :: AppConfigFeatures
editedFeatures = defaultFeatures {_appConfigFeaturesRegistration = SimpleFeature False}

editedClient :: AppConfigClient
editedClient = defaultClient {_appConfigClientPrivacyUrl = "https://ds-wizard.org/privacy.html/EDITED"}
