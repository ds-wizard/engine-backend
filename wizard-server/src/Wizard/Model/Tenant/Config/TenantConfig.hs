module Wizard.Model.Tenant.Config.TenantConfig where

import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Config.SimpleFeature
import Shared.OpenId.Model.OpenId.OpenIdClientParameter
import Shared.OpenId.Model.OpenId.OpenIdClientStyle
import Wizard.Model.Questionnaire.Questionnaire hiding (uuid)
import WizardLib.Public.Model.Tenant.Config.TenantConfig

data TenantConfig = TenantConfig
  { uuid :: U.UUID
  , organization :: TenantConfigOrganization
  , authentication :: TenantConfigAuthentication
  , privacyAndSupport :: TenantConfigPrivacyAndSupport
  , dashboardAndLoginScreen :: TenantConfigDashboardAndLoginScreen
  , lookAndFeel :: TenantConfigLookAndFeel
  , registry :: TenantConfigRegistry
  , knowledgeModel :: TenantConfigKnowledgeModel
  , questionnaire :: TenantConfigQuestionnaire
  , submission :: TenantConfigSubmission
  , features :: TenantConfigFeatures
  , owl :: TenantConfigOwl
  , mailConfigUuid :: Maybe U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfig where
  a == b =
    uuid a == uuid b
      && organization a == organization b
      && authentication a == authentication b
      && privacyAndSupport a == privacyAndSupport b
      && dashboardAndLoginScreen a == dashboardAndLoginScreen b
      && lookAndFeel a == lookAndFeel b
      && registry a == registry b
      && questionnaire a == questionnaire b
      && submission a == submission b
      && features a == features b
      && owl a == owl b
      && mailConfigUuid a == mailConfigUuid b

data TenantConfigOrganization = TenantConfigOrganization
  { tenantUuid :: U.UUID
  , name :: String
  , description :: String
  , organizationId :: String
  , affiliations :: [String]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigOrganization where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.name == b.name
      && a.description == b.description
      && a.organizationId == b.organizationId
      && a.affiliations == b.affiliations

instance Hashable TenantConfigOrganization

data TenantConfigAuthentication = TenantConfigAuthentication
  { tenantUuid :: U.UUID
  , defaultRole :: String
  , internal :: TenantConfigAuthenticationInternal
  , external :: TenantConfigAuthenticationExternal
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigAuthentication where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.defaultRole == b.defaultRole
      && a.internal == b.internal
      && a.external == b.external

data TenantConfigAuthenticationInternal = TenantConfigAuthenticationInternal
  { registration :: SimpleFeature
  , twoFactorAuth :: TenantConfigAuthenticationInternalTwoFactorAuth
  }
  deriving (Generic, Eq, Show)

data TenantConfigAuthenticationInternalTwoFactorAuth = TenantConfigAuthenticationInternalTwoFactorAuth
  { enabled :: Bool
  , codeLength :: Int
  , expiration :: Int
  }
  deriving (Generic, Eq, Show)

data TenantConfigAuthenticationExternal = TenantConfigAuthenticationExternal
  { services :: [TenantConfigAuthenticationExternalService]
  }
  deriving (Generic, Eq, Show)

data TenantConfigAuthenticationExternalService = TenantConfigAuthenticationExternalService
  { tenantUuid :: U.UUID
  , aId :: String
  , name :: String
  , url :: String
  , clientId :: String
  , clientSecret :: String
  , parameters :: [OpenIdClientParameter]
  , style :: OpenIdClientStyle
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigAuthenticationExternalService where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.aId == b.aId
      && a.name == b.name
      && a.url == b.url
      && a.clientId == b.clientId
      && a.clientSecret == b.clientSecret
      && a.parameters == b.parameters
      && a.style == b.style

data TenantConfigPrivacyAndSupport = TenantConfigPrivacyAndSupport
  { tenantUuid :: U.UUID
  , privacyUrl :: Maybe String
  , termsOfServiceUrl :: Maybe String
  , supportEmail :: Maybe String
  , supportSiteName :: Maybe String
  , supportSiteUrl :: Maybe String
  , supportSiteIcon :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigPrivacyAndSupport where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.privacyUrl == b.privacyUrl
      && a.termsOfServiceUrl == b.termsOfServiceUrl
      && a.supportEmail == b.supportEmail
      && a.supportSiteName == b.supportSiteName
      && a.supportSiteUrl == b.supportSiteUrl
      && a.supportSiteIcon == b.supportSiteIcon

data TenantConfigDashboardAndLoginScreen = TenantConfigDashboardAndLoginScreen
  { tenantUuid :: U.UUID
  , dashboardType :: TenantConfigDashboardAndLoginScreenDashboardType
  , announcements :: [TenantConfigDashboardAndLoginScreenAnnouncement]
  , loginInfo :: Maybe String
  , loginInfoSidebar :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigDashboardAndLoginScreen where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.dashboardType == b.dashboardType
      && a.announcements == b.announcements
      && a.loginInfo == b.loginInfo
      && a.loginInfoSidebar == b.loginInfoSidebar

data TenantConfigDashboardAndLoginScreenDashboardType
  = WelcomeDashboardType
  | RoleBasedDashboardType
  deriving (Generic, Eq, Show, Read)

data TenantConfigRegistry = TenantConfigRegistry
  { tenantUuid :: U.UUID
  , enabled :: Bool
  , token :: String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigRegistry where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.enabled == b.enabled
      && a.token == b.token

data TenantConfigKnowledgeModel = TenantConfigKnowledgeModel
  { tenantUuid :: U.UUID
  , public :: TenantConfigKnowledgeModelPublic
  , integrationConfig :: String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigKnowledgeModel where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.public == b.public
      && a.integrationConfig == b.integrationConfig

data TenantConfigKnowledgeModelPublic = TenantConfigKnowledgeModelPublic
  { enabled :: Bool
  , knowledgeModelPackages :: [TenantConfigKnowledgeModelPublicPackagePattern]
  }
  deriving (Generic, Eq, Show)

data TenantConfigKnowledgeModelPublicPackagePattern = TenantConfigKnowledgeModelPublicPackagePattern
  { tenantUuid :: U.UUID
  , position :: Int
  , orgId :: Maybe String
  , kmId :: Maybe String
  , minVersion :: Maybe String
  , maxVersion :: Maybe String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigKnowledgeModelPublicPackagePattern where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.orgId == b.orgId
      && a.kmId == b.kmId
      && a.minVersion == b.minVersion
      && a.maxVersion == b.maxVersion

data TenantConfigQuestionnaire = TenantConfigQuestionnaire
  { tenantUuid :: U.UUID
  , questionnaireVisibility :: TenantConfigQuestionnaireVisibility
  , questionnaireSharing :: TenantConfigQuestionnaireSharing
  , questionnaireCreation :: QuestionnaireCreation
  , projectTagging :: TenantConfigQuestionnaireProjectTagging
  , summaryReport :: SimpleFeature
  , feedback :: TenantConfigQuestionnaireFeedback
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigQuestionnaire where
  a == b =
    a.questionnaireVisibility == b.questionnaireVisibility
      && a.questionnaireSharing == b.questionnaireSharing
      && a.questionnaireCreation == b.questionnaireCreation
      && a.projectTagging == b.projectTagging
      && a.summaryReport == b.summaryReport
      && a.feedback == b.feedback

data TenantConfigQuestionnaireVisibility = TenantConfigQuestionnaireVisibility
  { enabled :: Bool
  , defaultValue :: QuestionnaireVisibility
  }
  deriving (Generic, Eq, Show)

data TenantConfigQuestionnaireSharing = TenantConfigQuestionnaireSharing
  { enabled :: Bool
  , defaultValue :: QuestionnaireSharing
  , anonymousEnabled :: Bool
  }
  deriving (Generic, Eq, Show)

data QuestionnaireCreation
  = CustomQuestionnaireCreation
  | TemplateQuestionnaireCreation
  | TemplateAndCustomQuestionnaireCreation
  deriving (Generic, Eq, Show, Read)

data TenantConfigQuestionnaireProjectTagging = TenantConfigQuestionnaireProjectTagging
  { enabled :: Bool
  , tags :: [String]
  }
  deriving (Generic, Eq, Show)

data TenantConfigQuestionnaireFeedback = TenantConfigQuestionnaireFeedback
  { enabled :: Bool
  , token :: String
  , owner :: String
  , repo :: String
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmission = TenantConfigSubmission
  { tenantUuid :: U.UUID
  , enabled :: Bool
  , services :: [TenantConfigSubmissionService]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigSubmission where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.enabled == b.enabled
      && a.services == b.services

data TenantConfigSubmissionService = TenantConfigSubmissionService
  { tenantUuid :: U.UUID
  , sId :: String
  , name :: String
  , description :: String
  , props :: [String]
  , supportedFormats :: [TenantConfigSubmissionServiceSupportedFormat]
  , request :: TenantConfigSubmissionServiceRequest
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigSubmissionService where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.sId == b.sId
      && a.name == b.name
      && a.description == b.description
      && a.props == b.props
      && a.supportedFormats == b.supportedFormats
      && a.request == b.request

data TenantConfigSubmissionServiceSupportedFormat = TenantConfigSubmissionServiceSupportedFormat
  { tenantUuid :: U.UUID
  , serviceId :: String
  , templateId :: String
  , formatUuid :: U.UUID
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmissionServiceRequest = TenantConfigSubmissionServiceRequest
  { method :: String
  , url :: String
  , headers :: M.Map String String
  , multipart :: TenantConfigSubmissionServiceRequestMultipart
  }
  deriving (Generic, Eq, Show)

data TenantConfigSubmissionServiceRequestMultipart = TenantConfigSubmissionServiceRequestMultipart
  { enabled :: Bool
  , fileName :: String
  }
  deriving (Generic, Eq, Show)

data TenantConfigOwl = TenantConfigOwl
  { tenantUuid :: U.UUID
  , enabled :: Bool
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , previousKnowledgeModelPackageId :: Maybe String
  , rootElement :: String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq TenantConfigOwl where
  a == b =
    a.tenantUuid == b.tenantUuid
      && a.enabled == b.enabled
      && a.name == b.name
      && a.organizationId == b.organizationId
      && a.kmId == b.kmId
      && a.version == b.version
      && a.previousKnowledgeModelPackageId == b.previousKnowledgeModelPackageId
      && a.rootElement == b.rootElement
