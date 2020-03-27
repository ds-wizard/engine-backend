module Wizard.Api.Resource.Config.ClientConfigDTO where

import GHC.Generics

import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Api.Resource.Config.SimpleFeatureDTO

data ClientConfigDTO =
  ClientConfigDTO
    { _clientConfigDTOFeatures :: ClientConfigFeaturesDTO
    , _clientConfigDTOClient :: AppConfigClientDTO
    , _clientConfigDTOInfo :: AppConfigInfoDTO
    , _clientConfigDTOAffiliation :: AppConfigAffiliationDTO
    , _clientConfigDTOAuth :: ClientConfigAuthDTO
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

data ClientConfigAuthDTO =
  ClientConfigAuthDTO
    { _clientConfigAuthDTOInternal :: AppConfigAuthInternalDTO
    , _clientConfigAuthDTOExternal :: ClientConfigAuthExternalDTO
    }
  deriving (Generic, Eq, Show)

data ClientConfigAuthExternalDTO =
  ClientConfigAuthExternalDTO
    { _clientConfigAuthExternalDTOServices :: [ClientConfigAuthExternalServiceDTO]
    }
  deriving (Generic, Eq, Show)

data ClientConfigAuthExternalServiceDTO =
  ClientConfigAuthExternalServiceDTO
    { _clientConfigAuthExternalServiceDTOAId :: String
    , _clientConfigAuthExternalServiceDTOName :: String
    , _clientConfigAuthExternalServiceDTOUrl :: String
    , _clientConfigAuthExternalServiceDTOStyle :: AppConfigAuthExternalServiceStyleDTO
    }
  deriving (Generic, Eq, Show)
