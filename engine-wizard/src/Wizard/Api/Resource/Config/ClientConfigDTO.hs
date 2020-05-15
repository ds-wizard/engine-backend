module Wizard.Api.Resource.Config.ClientConfigDTO where

import GHC.Generics

import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.SimpleFeature
import Wizard.Model.User.User

data ClientConfigDTO =
  ClientConfigDTO
    { _clientConfigDTOOrganization :: AppConfigOrganization
    , _clientConfigDTOAuthentication :: ClientConfigAuthDTO
    , _clientConfigDTOPrivacyAndSupport :: AppConfigPrivacyAndSupport
    , _clientConfigDTODashboard :: AppConfigDashboard
    , _clientConfigDTOLookAndFeel :: AppConfigLookAndFeel
    , _clientConfigDTOKnowledgeModelRegistry :: ClientConfigRegistryDTO
    , _clientConfigDTOQuestionnaire :: ClientConfigQuestionnaireDTO
    , _clientConfigDTOTemplate :: AppConfigTemplate
    , _clientConfigDTOSubmission :: SimpleFeature
    }
  deriving (Show, Eq, Generic)

data ClientConfigAuthDTO =
  ClientConfigAuthDTO
    { _clientConfigAuthDTODefaultRole :: Role
    , _clientConfigAuthDTOInternal :: AppConfigAuthInternal
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
    , _clientConfigAuthExternalServiceDTOStyle :: Maybe AppConfigAuthExternalServiceStyle
    }
  deriving (Generic, Eq, Show)

data ClientConfigRegistryDTO =
  ClientConfigRegistryDTO
    { _clientConfigRegistryDTOEnabled :: Bool
    , _clientConfigRegistryDTOUrl :: String
    }
  deriving (Show, Eq, Generic)

data ClientConfigQuestionnaireDTO =
  ClientConfigQuestionnaireDTO
    { _clientConfigQuestionnaireDTOQuestionnaireAccessibility :: SimpleFeature
    , _clientConfigQuestionnaireDTOLevels :: SimpleFeature
    , _clientConfigQuestionnaireDTOFeedback :: SimpleFeature
    }
  deriving (Generic, Eq, Show)
