module Wizard.Model.Tenant.Config.TenantConfig where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Model.Config.SimpleFeature
import Shared.OpenId.Model.OpenId.OpenIdClientParameter
import Shared.OpenId.Model.OpenId.OpenIdClientStyle
import Wizard.Model.Questionnaire.Questionnaire hiding (uuid)
import WizardLib.KnowledgeModel.Model.Package.PackagePattern
import WizardLib.Public.Model.Tenant.Config.TenantConfig

data TenantConfig = TenantConfig
  { uuid :: U.UUID
  , organization :: TenantConfigOrganization
  , authentication :: TenantConfigAuth
  , privacyAndSupport :: TenantConfigPrivacyAndSupport
  , dashboardAndLoginScreen :: TenantConfigDashboardAndLoginScreen
  , lookAndFeel :: TenantConfigLookAndFeel
  , registry :: TenantConfigRegistry
  , knowledgeModel :: TenantConfigKnowledgeModel
  , questionnaire :: TenantConfigQuestionnaire
  , owl :: TenantConfigOwl
  , mailConfigUuid :: Maybe U.UUID
  , aiAssistant :: TenantConfigAiAssistant
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
      && owl a == owl b
      && mailConfigUuid a == mailConfigUuid b

data TenantConfigAuth = TenantConfigAuth
  { defaultRole :: String
  , internal :: TenantConfigAuthInternal
  , external :: TenantConfigAuthExternal
  }
  deriving (Generic, Eq, Show)

data TenantConfigAuthInternal = TenantConfigAuthInternal
  { registration :: SimpleFeature
  , twoFactorAuth :: TenantConfigAuthInternalTwoFactorAuth
  }
  deriving (Generic, Eq, Show)

data TenantConfigAuthInternalTwoFactorAuth = TenantConfigAuthInternalTwoFactorAuth
  { enabled :: Bool
  , codeLength :: Int
  , expiration :: Int
  }
  deriving (Generic, Eq, Show)

data TenantConfigAuthExternal = TenantConfigAuthExternal
  { services :: [TenantConfigAuthExternalService]
  }
  deriving (Generic, Eq, Show)

data TenantConfigAuthExternalService = TenantConfigAuthExternalService
  { aId :: String
  , name :: String
  , url :: String
  , clientId :: String
  , clientSecret :: String
  , parameteres :: [OpenIdClientParameter]
  , style :: Maybe OpenIdClientStyle
  }
  deriving (Generic, Eq, Show)

data TenantConfigPrivacyAndSupport = TenantConfigPrivacyAndSupport
  { privacyUrl :: Maybe String
  , termsOfServiceUrl :: Maybe String
  , supportEmail :: Maybe String
  , supportSiteName :: Maybe String
  , supportSiteUrl :: Maybe String
  , supportSiteIcon :: Maybe String
  }
  deriving (Generic, Eq, Show)

data TenantConfigDashboardAndLoginScreen = TenantConfigDashboardAndLoginScreen
  { dashboardType :: TenantConfigDashboardAndLoginScreenDashboardType
  , announcements :: [TenantConfigDashboardAndLoginScreenAnnouncement]
  , loginInfo :: Maybe String
  , loginInfoSidebar :: Maybe String
  }
  deriving (Generic, Eq, Show)

data TenantConfigDashboardAndLoginScreenDashboardType
  = WelcomeDashboardType
  | RoleBasedDashboardType
  deriving (Generic, Eq, Show)

data TenantConfigLookAndFeel = TenantConfigLookAndFeel
  { appTitle :: Maybe String
  , appTitleShort :: Maybe String
  , customMenuLinks :: [TenantConfigLookAndFeelCustomMenuLink]
  , logoUrl :: Maybe String
  , primaryColor :: Maybe String
  , illustrationsColor :: Maybe String
  }
  deriving (Generic, Eq, Show)

data TenantConfigLookAndFeelCustomMenuLink = TenantConfigLookAndFeelCustomMenuLink
  { icon :: String
  , title :: String
  , url :: String
  , newWindow :: Bool
  }
  deriving (Show, Eq, Generic)

data TenantConfigRegistry = TenantConfigRegistry
  { enabled :: Bool
  , token :: String
  }
  deriving (Generic, Eq, Show)

data TenantConfigKnowledgeModel = TenantConfigKnowledgeModel
  { public :: TenantConfigKnowledgeModelPublic
  , integrationConfig :: String
  }
  deriving (Generic, Eq, Show)

data TenantConfigKnowledgeModelPublic = TenantConfigKnowledgeModelPublic
  { enabled :: Bool
  , packages :: [PackagePattern]
  }
  deriving (Generic, Eq, Show)

data TenantConfigQuestionnaire = TenantConfigQuestionnaire
  { questionnaireVisibility :: TenantConfigQuestionnaireVisibility
  , questionnaireSharing :: TenantConfigQuestionnaireSharing
  , questionnaireCreation :: QuestionnaireCreation
  , projectTagging :: TenantConfigQuestionnaireProjectTagging
  , summaryReport :: SimpleFeature
  , feedback :: TenantConfigQuestionnaireFeedback
  }
  deriving (Generic, Eq, Show)

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
  deriving (Generic, Eq, Show)

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
data TenantConfigOwl = TenantConfigOwl
  { enabled :: Bool
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , previousPackageId :: Maybe String
  , rootElement :: String
  }
  deriving (Generic, Eq, Show)

data TenantConfigAiAssistant = TenantConfigAiAssistant
  { enabled :: Bool
  }
  deriving (Generic, Eq, Show)
