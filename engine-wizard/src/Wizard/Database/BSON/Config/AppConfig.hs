module Wizard.Database.BSON.Config.AppConfig where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Wizard.Database.BSON.Common ()
import Wizard.Database.BSON.Config.SimpleFeature ()
import Wizard.Model.Config.AppConfig

instance ToBSON AppConfig where
  toBSON AppConfig {..} =
    [ "features" BSON.=: _appConfigFeatures
    , "client" BSON.=: _appConfigClient
    , "createdAt" BSON.=: _appConfigCreatedAt
    , "updatedAt" BSON.=: _appConfigUpdatedAt
    ]

instance FromBSON AppConfig where
  fromBSON doc = do
    _appConfigFeatures <- BSON.lookup "features" doc
    _appConfigClient <- BSON.lookup "client" doc
    _appConfigCreatedAt <- BSON.lookup "createdAt" doc
    _appConfigUpdatedAt <- BSON.lookup "updatedAt" doc
    return AppConfig {..}

instance ToBSON AppConfigFeatures where
  toBSON AppConfigFeatures {..} =
    [ "registration" BSON.=: _appConfigFeaturesRegistration
    , "publicQuestionnaire" BSON.=: _appConfigFeaturesPublicQuestionnaire
    , "levels" BSON.=: _appConfigFeaturesLevels
    , "questionnaireAccessibility" BSON.=: _appConfigFeaturesQuestionnaireAccessibility
    ]

instance FromBSON AppConfigFeatures where
  fromBSON doc = do
    _appConfigFeaturesRegistration <- BSON.lookup "registration" doc
    _appConfigFeaturesPublicQuestionnaire <- BSON.lookup "publicQuestionnaire" doc
    _appConfigFeaturesLevels <- BSON.lookup "levels" doc
    _appConfigFeaturesQuestionnaireAccessibility <- BSON.lookup "questionnaireAccessibility" doc
    return AppConfigFeatures {..}

instance ToBSON AppConfigClient where
  toBSON AppConfigClient {..} =
    [ "privacyUrl" BSON.=: _appConfigClientPrivacyUrl
    , "appTitle" BSON.=: _appConfigClientAppTitle
    , "appTitleShort" BSON.=: _appConfigClientAppTitleShort
    , "welcomeWarning" BSON.=: _appConfigClientWelcomeWarning
    , "welcomeInfo" BSON.=: _appConfigClientWelcomeInfo
    , "loginInfo" BSON.=: _appConfigClientLoginInfo
    , "supportEmail" BSON.=: _appConfigClientSupportEmail
    , "supportRepositoryName" BSON.=: _appConfigClientSupportRepositoryName
    , "supportRepositoryUrl" BSON.=: _appConfigClientSupportRepositoryUrl
    , "dashboard" BSON.=: _appConfigClientDashboard
    , "customMenuLinks" BSON.=: _appConfigClientCustomMenuLinks
    ]

instance FromBSON AppConfigClient where
  fromBSON doc = do
    _appConfigClientPrivacyUrl <- BSON.lookup "privacyUrl" doc
    _appConfigClientAppTitle <- BSON.lookup "appTitle" doc
    _appConfigClientAppTitleShort <- BSON.lookup "appTitleShort" doc
    _appConfigClientWelcomeWarning <- BSON.lookup "welcomeWarning" doc
    _appConfigClientWelcomeInfo <- BSON.lookup "welcomeInfo" doc
    _appConfigClientLoginInfo <- BSON.lookup "loginInfo" doc
    _appConfigClientSupportEmail <- BSON.lookup "supportEmail" doc
    _appConfigClientSupportRepositoryName <- BSON.lookup "supportRepositoryName" doc
    _appConfigClientSupportRepositoryUrl <- BSON.lookup "supportRepositoryUrl" doc
    let _appConfigClientDashboard = BSON.lookup "dashboard" doc
    _appConfigClientCustomMenuLinks <- BSON.lookup "customMenuLinks" doc
    return AppConfigClient {..}

instance ToBSON AppConfigClientDashboard where
  toBSON AppConfigClientDashboard {..} =
    [ "dashboardAdmin" BSON.=: _appConfigClientDashboardAdmin
    , "dashboardDataSteward" BSON.=: _appConfigClientDashboardDataSteward
    , "dashboardResearcher" BSON.=: _appConfigClientDashboardResearcher
    ]

instance FromBSON AppConfigClientDashboard where
  fromBSON doc = do
    _appConfigClientDashboardAdmin <- BSON.lookup "dashboardAdmin" doc
    _appConfigClientDashboardDataSteward <- BSON.lookup "dashboardDataSteward" doc
    _appConfigClientDashboardResearcher <- BSON.lookup "dashboardResearcher" doc
    return AppConfigClientDashboard {..}

instance ToBSON AppConfigClientCustomMenuLink where
  toBSON AppConfigClientCustomMenuLink {..} =
    [ "icon" BSON.=: _appConfigClientCustomMenuLinkIcon
    , "title" BSON.=: _appConfigClientCustomMenuLinkTitle
    , "url" BSON.=: _appConfigClientCustomMenuLinkUrl
    , "newWindow" BSON.=: _appConfigClientCustomMenuLinkNewWindow
    ]

instance FromBSON AppConfigClientCustomMenuLink where
  fromBSON doc = do
    _appConfigClientCustomMenuLinkIcon <- BSON.lookup "icon" doc
    _appConfigClientCustomMenuLinkTitle <- BSON.lookup "title" doc
    _appConfigClientCustomMenuLinkUrl <- BSON.lookup "url" doc
    _appConfigClientCustomMenuLinkNewWindow <- BSON.lookup "newWindow" doc
    return AppConfigClientCustomMenuLink {..}
