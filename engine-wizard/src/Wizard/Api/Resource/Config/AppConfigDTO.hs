module Wizard.Api.Resource.Config.AppConfigDTO where

import GHC.Generics

import Wizard.Api.Resource.Config.SimpleFeatureDTO

data AppConfigFeaturesDTO =
  AppConfigFeaturesDTO
    { _appConfigFeaturesDTORegistration :: SimpleFeatureDTO
    , _appConfigFeaturesDTOPublicQuestionnaire :: SimpleFeatureDTO
    , _appConfigFeaturesDTOLevels :: SimpleFeatureDTO
    , _appConfigFeaturesDTOQuestionnaireAccessibility :: SimpleFeatureDTO
    }
  deriving (Generic, Eq, Show)

data AppConfigClientDTO =
  AppConfigClientDTO
    { _appConfigClientDTOPrivacyUrl :: String
    , _appConfigClientDTOAppTitle :: Maybe String
    , _appConfigClientDTOAppTitleShort :: Maybe String
    , _appConfigClientDTOSupportEmail :: Maybe String
    , _appConfigClientDTOSupportRepositoryName :: Maybe String
    , _appConfigClientDTOSupportRepositoryUrl :: Maybe String
    , _appConfigClientDTODashboard :: Maybe AppConfigClientDashboardDTO
    , _appConfigClientDTOCustomMenuLinks :: [AppConfigClientCustomMenuLinkDTO]
    }
  deriving (Generic, Eq, Show)

data AppConfigClientDashboardDTO =
  AppConfigClientDashboardDTO
    { _appConfigClientDashboardDTOAdmin :: [String]
    , _appConfigClientDashboardDTODataSteward :: [String]
    , _appConfigClientDashboardDTOResearcher :: [String]
    }
  deriving (Generic, Eq, Show)

data AppConfigClientCustomMenuLinkDTO =
  AppConfigClientCustomMenuLinkDTO
    { _appConfigClientCustomMenuLinkDTOIcon :: String
    , _appConfigClientCustomMenuLinkDTOTitle :: String
    , _appConfigClientCustomMenuLinkDTOUrl :: String
    , _appConfigClientCustomMenuLinkDTONewWindow :: Bool
    }
  deriving (Show, Eq, Generic)

data AppConfigInfoDTO =
  AppConfigInfoDTO
    { _appConfigInfoDTOWelcomeWarning :: Maybe String
    , _appConfigInfoDTOWelcomeInfo :: Maybe String
    , _appConfigInfoDTOLoginInfo :: Maybe String
    }
  deriving (Generic, Eq, Show)

data AppConfigAffiliationDTO =
  AppConfigAffiliationDTO
    { _appConfigAffiliationDTOAffiliations :: [String]
    }
  deriving (Generic, Eq, Show)
