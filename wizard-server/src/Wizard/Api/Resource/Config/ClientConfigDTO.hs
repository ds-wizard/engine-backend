module Wizard.Api.Resource.Config.ClientConfigDTO where

import GHC.Generics

import Shared.Common.Model.Config.SimpleFeature
import Shared.OpenId.Model.OpenId.OpenIdClientStyle
import Wizard.Model.Config.AppConfig

data ClientConfigDTO = ClientConfigDTO
  { organization :: AppConfigOrganization
  , feature :: AppConfigFeature
  , authentication :: ClientConfigAuthDTO
  , privacyAndSupport :: AppConfigPrivacyAndSupport
  , dashboardAndLoginScreen :: AppConfigDashboardAndLoginScreen
  , lookAndFeel :: AppConfigLookAndFeel
  , registry :: ClientConfigRegistryDTO
  , questionnaire :: ClientConfigQuestionnaireDTO
  , submission :: SimpleFeature
  , cloud :: ClientConfigCloudDTO
  , locales :: [ClientConfigLocaleDTO]
  , owl :: AppConfigOwl
  , admin :: ClientConfigAdminDTO
  , modules :: [ClientConfigModuleDTO]
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
  , style :: Maybe OpenIdClientStyle
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

data ClientConfigAdminDTO = ClientConfigAdminDTO
  { enabled :: Bool
  , clientUrl :: Maybe String
  }
  deriving (Generic, Eq, Show)

data ClientConfigModuleDTO = ClientConfigModuleDTO
  { title :: String
  , description :: String
  , icon :: String
  , url :: String
  , external :: Bool
  }
  deriving (Generic, Eq, Show)
