module Wizard.Api.Resource.Config.AppConfigDTO where

import GHC.Generics

import Wizard.Api.Resource.Config.SimpleFeatureDTO

data AppConfigFeaturesDTO =
  AppConfigFeaturesDTO
    { _appConfigFeaturesDTOPublicQuestionnaire :: SimpleFeatureDTO
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

data AppConfigAuthDTO =
  AppConfigAuthDTO
    { _appConfigAuthDTOInternal :: AppConfigAuthInternalDTO
    , _appConfigAuthDTOExternal :: AppConfigAuthExternalDTO
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthInternalDTO =
  AppConfigAuthInternalDTO
    { _appConfigAuthInternalDTORegistration :: SimpleFeatureDTO
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternalDTO =
  AppConfigAuthExternalDTO
    { _appConfigAuthExternalDTOServices :: [AppConfigAuthExternalServiceDTO]
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternalServiceDTO =
  AppConfigAuthExternalServiceDTO
    { _appConfigAuthExternalServiceDTOAId :: String
    , _appConfigAuthExternalServiceDTOName :: String
    , _appConfigAuthExternalServiceDTOUrl :: String
    , _appConfigAuthExternalServiceDTOClientId :: String
    , _appConfigAuthExternalServiceDTOClientSecret :: String
    , _appConfigAuthExternalServiceDTOParameters :: [AppConfigAuthExternalServiceParameterDTO]
    , _appConfigAuthExternalServiceDTOStyle :: AppConfigAuthExternalServiceStyleDTO
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternalServiceParameterDTO =
  AppConfigAuthExternalServiceParameterDTO
    { _appConfigAuthExternalServiceParameterDTOName :: String
    , _appConfigAuthExternalServiceParameterDTOValue :: String
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternalServiceStyleDTO =
  AppConfigAuthExternalServiceStyleDTO
    { _appConfigAuthExternalServiceStyleDTOIcon :: String
    , _appConfigAuthExternalServiceStyleDTOBackground :: String
    , _appConfigAuthExternalServiceStyleDTOColor :: String
    }
  deriving (Generic, Eq, Show)
