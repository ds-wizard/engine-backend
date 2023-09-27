module Wizard.Api.Resource.Config.ClientConfigDTO where

import GHC.Generics

import Shared.Common.Model.Config.SimpleFeature
import Shared.OpenId.Model.OpenId.OpenIdClientStyle
import Wizard.Model.Tenant.Config.TenantConfig

data ClientConfigDTO = ClientConfigDTO
  { organization :: TenantConfigOrganization
  , feature :: TenantConfigFeature
  , authentication :: ClientConfigAuthDTO
  , privacyAndSupport :: TenantConfigPrivacyAndSupport
  , dashboardAndLoginScreen :: TenantConfigDashboardAndLoginScreen
  , lookAndFeel :: TenantConfigLookAndFeel
  , registry :: ClientConfigRegistryDTO
  , questionnaire :: ClientConfigQuestionnaireDTO
  , submission :: SimpleFeature
  , cloud :: ClientConfigCloudDTO
  , locales :: [ClientConfigLocaleDTO]
  , owl :: TenantConfigOwl
  , admin :: ClientConfigAdminDTO
  , modules :: [ClientConfigModuleDTO]
  }
  deriving (Show, Eq, Generic)

data ClientConfigAuthDTO = ClientConfigAuthDTO
  { defaultRole :: String
  , internal :: TenantConfigAuthInternal
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
  { questionnaireVisibility :: TenantConfigQuestionnaireVisibility
  , questionnaireSharing :: TenantConfigQuestionnaireSharing
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
