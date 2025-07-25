module Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Config.SimpleFeature
import Shared.OpenId.Model.OpenId.OpenIdClientParameter
import Shared.OpenId.Model.OpenId.OpenIdClientStyle
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.KnowledgeModel.Model.Package.PackagePattern
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigChangeDTO

data TenantConfigChangeDTO = TenantConfigChangeDTO
  { organization :: TenantConfigOrganizationChangeDTO
  , authentication :: TenantConfigAuthenticationChangeDTO
  , privacyAndSupport :: TenantConfigPrivacyAndSupportChangeDTO
  , dashboardAndLoginScreen :: TenantConfigDashboardAndLoginScreenChangeDTO
  , lookAndFeel :: TenantConfigLookAndFeelChangeDTO
  , registry :: TenantConfigRegistryChangeDTO
  , knowledgeModel :: TenantConfigKnowledgeModelChangeDTO
  , questionnaire :: TenantConfigQuestionnaireChangeDTO
  , submission :: TenantConfigSubmissionChangeDTO
  , features :: TenantConfigFeaturesChangeDTO
  }
  deriving (Generic, Show)

data TenantConfigOrganizationChangeDTO = TenantConfigOrganizationChangeDTO
  { name :: String
  , description :: String
  , organizationId :: String
  , affiliations :: [String]
  }
  deriving (Generic, Eq, Show)

data TenantConfigAuthenticationChangeDTO = TenantConfigAuthenticationChangeDTO
  { defaultRole :: String
  , internal :: TenantConfigAuthenticationInternal
  , external :: TenantConfigAuthenticationExternalChangeDTO
  }
  deriving (Generic, Eq, Show)

data TenantConfigAuthenticationExternalChangeDTO = TenantConfigAuthenticationExternalChangeDTO
  { services :: [TenantConfigAuthenticationExternalServiceChangeDTO]
  }
  deriving (Generic, Eq, Show)

data TenantConfigAuthenticationExternalServiceChangeDTO = TenantConfigAuthenticationExternalServiceChangeDTO
  { aId :: String
  , name :: String
  , url :: String
  , clientId :: String
  , clientSecret :: String
  , parameters :: [OpenIdClientParameter]
  , style :: OpenIdClientStyle
  }
  deriving (Generic, Eq, Show)

data TenantConfigPrivacyAndSupportChangeDTO = TenantConfigPrivacyAndSupportChangeDTO
  { privacyUrl :: Maybe String
  , termsOfServiceUrl :: Maybe String
  , supportEmail :: Maybe String
  , supportSiteName :: Maybe String
  , supportSiteUrl :: Maybe String
  , supportSiteIcon :: Maybe String
  }
  deriving (Generic, Eq, Show)

data TenantConfigDashboardAndLoginScreenChangeDTO = TenantConfigDashboardAndLoginScreenChangeDTO
  { dashboardType :: TenantConfigDashboardAndLoginScreenDashboardType
  , announcements :: [TenantConfigDashboardAndLoginScreenAnnouncementChangeDTO]
  , loginInfo :: Maybe String
  , loginInfoSidebar :: Maybe String
  }
  deriving (Generic, Eq, Show)

data TenantConfigRegistryChangeDTO = TenantConfigRegistryChangeDTO
  { enabled :: Bool
  , token :: String
  }
  deriving (Generic, Eq, Show)

data TenantConfigKnowledgeModelChangeDTO = TenantConfigKnowledgeModelChangeDTO
  { public :: TenantConfigKnowledgeModelPublicChangeDTO
  , integrationConfig :: String
  }
  deriving (Generic, Show)
data TenantConfigKnowledgeModelPublicChangeDTO = TenantConfigKnowledgeModelPublicChangeDTO
  { enabled :: Bool
  , packages :: [PackagePattern]
  }
  deriving (Generic, Show)

data TenantConfigQuestionnaireChangeDTO = TenantConfigQuestionnaireChangeDTO
  { questionnaireVisibility :: TenantConfigQuestionnaireVisibility
  , questionnaireSharing :: TenantConfigQuestionnaireSharing
  , questionnaireCreation :: QuestionnaireCreation
  , projectTagging :: TenantConfigQuestionnaireProjectTagging
  , summaryReport :: SimpleFeature
  , feedback :: TenantConfigQuestionnaireFeedback
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmissionChangeDTO = TenantConfigSubmissionChangeDTO
  { enabled :: Bool
  , services :: [TenantConfigSubmissionServiceChangeDTO]
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmissionServiceChangeDTO = TenantConfigSubmissionServiceChangeDTO
  { sId :: String
  , name :: String
  , description :: String
  , props :: [String]
  , supportedFormats :: [TenantConfigSubmissionServiceSupportedFormatChangeDTO]
  , request :: TenantConfigSubmissionServiceRequestChangeDTO
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmissionServiceSupportedFormatChangeDTO = TenantConfigSubmissionServiceSupportedFormatChangeDTO
  { templateId :: String
  , formatUuid :: U.UUID
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmissionServiceRequestChangeDTO = TenantConfigSubmissionServiceRequestChangeDTO
  { method :: String
  , url :: String
  , headers :: M.Map String String
  , multipart :: TenantConfigSubmissionServiceRequestMultipartChangeDTO
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmissionServiceRequestMultipartChangeDTO = TenantConfigSubmissionServiceRequestMultipartChangeDTO
  { enabled :: Bool
  , fileName :: String
  }
  deriving (Generic, Eq, Show)

data TenantConfigFeaturesChangeDTO = TenantConfigFeaturesChangeDTO
  { toursEnabled :: Bool
  }
  deriving (Generic, Eq, Show)
