module Wizard.Api.Resource.Config.ClientConfigDTO where

import GHC.Generics

import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.SimpleFeature

data ClientConfigDTO = ClientConfigDTO
  { organization :: AppConfigOrganization
  , feature :: AppConfigFeature
  , authentication :: ClientConfigAuthDTO
  , privacyAndSupport :: AppConfigPrivacyAndSupport
  , dashboard :: AppConfigDashboard
  , lookAndFeel :: AppConfigLookAndFeel
  , registry :: ClientConfigRegistryDTO
  , questionnaire :: ClientConfigQuestionnaireDTO
  , submission :: SimpleFeature
  , cloud :: ClientConfigCloudDTO
  , locales :: [ClientConfigLocaleDTO]
  , owl :: AppConfigOwl
  }
  deriving (Show, Eq, Generic)

data ClientConfigAuthDTO = ClientConfigAuthDTO
  { defaultRole :: String
  , internal :: AppConfigAuthInternal
  , external :: ClientConfigAuthExternalDTO
  }
  deriving (Generic, Eq, Show)

data ClientConfigAuthExternalDTO = ClientConfigAuthExternalDTO
  { services :: [ClientConfigAuthExternalServiceDTO]
  }
  deriving (Generic, Eq, Show)

data ClientConfigAuthExternalServiceDTO = ClientConfigAuthExternalServiceDTO
  { aId :: String
  , name :: String
  , url :: String
  , style :: Maybe AppConfigAuthExternalServiceStyle
  }
  deriving (Generic, Eq, Show)

data ClientConfigRegistryDTO = ClientConfigRegistryDTO
  { enabled :: Bool
  , url :: String
  }
  deriving (Show, Eq, Generic)

data ClientConfigQuestionnaireDTO = ClientConfigQuestionnaireDTO
  { questionnaireVisibility :: AppConfigQuestionnaireVisibility
  , questionnaireSharing :: AppConfigQuestionnaireSharing
  , questionnaireCreation :: QuestionnaireCreation
  , projectTagging :: SimpleFeature
  , summaryReport :: SimpleFeature
  , feedback :: SimpleFeature
  }
  deriving (Generic, Eq, Show)

data ClientConfigCloudDTO = ClientConfigCloudDTO
  { enabled :: Bool
  , serverUrl :: String
  }
  deriving (Generic, Eq, Show)

data ClientConfigLocaleDTO = ClientConfigLocaleDTO
  { name :: String
  , code :: String
  , defaultLocale :: Bool
  }
  deriving (Show, Eq, Generic)
