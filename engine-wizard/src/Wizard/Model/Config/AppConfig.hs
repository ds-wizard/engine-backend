module Wizard.Model.Config.AppConfig where

import Data.Time
import GHC.Generics

import Wizard.Model.Config.SimpleFeature

data AppConfig =
  AppConfig
    { _appConfigFeatures :: AppConfigFeatures
    , _appConfigClient :: AppConfigClient
    , _appConfigInfo :: AppConfigInfo
    , _appConfigAffiliation :: AppConfigAffiliation
    , _appConfigCreatedAt :: UTCTime
    , _appConfigUpdatedAt :: UTCTime
    }
  deriving (Generic, Show)

instance Eq AppConfig where
  a == b = _appConfigFeatures a == _appConfigFeatures b && _appConfigClient a == _appConfigClient b

data AppConfigFeatures =
  AppConfigFeatures
    { _appConfigFeaturesRegistration :: SimpleFeature
    , _appConfigFeaturesPublicQuestionnaire :: SimpleFeature
    , _appConfigFeaturesLevels :: SimpleFeature
    , _appConfigFeaturesQuestionnaireAccessibility :: SimpleFeature
    }
  deriving (Generic, Eq, Show)

data AppConfigClient =
  AppConfigClient
    { _appConfigClientPrivacyUrl :: String
    , _appConfigClientAppTitle :: Maybe String
    , _appConfigClientAppTitleShort :: Maybe String
    , _appConfigClientSupportEmail :: Maybe String
    , _appConfigClientSupportRepositoryName :: Maybe String
    , _appConfigClientSupportRepositoryUrl :: Maybe String
    , _appConfigClientDashboard :: Maybe AppConfigClientDashboard
    , _appConfigClientCustomMenuLinks :: [AppConfigClientCustomMenuLink]
    }
  deriving (Generic, Eq, Show)

data AppConfigClientDashboard =
  AppConfigClientDashboard
    { _appConfigClientDashboardAdmin :: [String]
    , _appConfigClientDashboardDataSteward :: [String]
    , _appConfigClientDashboardResearcher :: [String]
    }
  deriving (Generic, Eq, Show)

data AppConfigClientCustomMenuLink =
  AppConfigClientCustomMenuLink
    { _appConfigClientCustomMenuLinkIcon :: String
    , _appConfigClientCustomMenuLinkTitle :: String
    , _appConfigClientCustomMenuLinkUrl :: String
    , _appConfigClientCustomMenuLinkNewWindow :: Bool
    }
  deriving (Show, Eq, Generic)

data AppConfigInfo =
  AppConfigInfo
    { _appConfigInfoWelcomeWarning :: Maybe String
    , _appConfigInfoWelcomeInfo :: Maybe String
    , _appConfigInfoLoginInfo :: Maybe String
    }
  deriving (Generic, Eq, Show)

data AppConfigAffiliation =
  AppConfigAffiliation
    { _appConfigAffiliationAffiliations :: [String]
    }
  deriving (Generic, Eq, Show)
