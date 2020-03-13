module Wizard.Api.Resource.Config.ClientConfigDTO where

import GHC.Generics

import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Api.Resource.Config.SimpleFeatureDTO

data ClientConfigDTO =
  ClientConfigDTO
    { _clientConfigDTOFeatures :: ClientConfigFeaturesDTO
    , _clientConfigDTOClient :: AppConfigClientDTO
    }
  deriving (Show, Eq, Generic)

data ClientConfigFeaturesDTO =
  ClientConfigFeaturesDTO
    { _clientConfigFeaturesDTORegistration :: SimpleFeatureDTO
    , _clientConfigFeaturesDTOPublicQuestionnaire :: SimpleFeatureDTO
    , _clientConfigFeaturesDTOLevels :: SimpleFeatureDTO
    , _clientConfigFeaturesDTOQuestionnaireAccessibility :: SimpleFeatureDTO
    , _clientConfigFeaturesDTOFeedback :: SimpleFeatureDTO
    , _clientConfigFeaturesDTORegistry :: ClientConfigRegistryDTO
    }
  deriving (Show, Eq, Generic)

data ClientConfigRegistryDTO =
  ClientConfigRegistryDTO
    { _clientConfigRegistryDTOEnabled :: Bool
    , _clientConfigRegistryDTOUrl :: String
    }
  deriving (Show, Eq, Generic)
