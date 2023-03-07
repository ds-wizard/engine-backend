module Wizard.Model.Config.AppConfig where

import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Package.PackagePattern
import Wizard.Model.Config.SimpleFeature
import Wizard.Model.Questionnaire.Questionnaire hiding (uuid)

data AppConfig = AppConfig
  { uuid :: U.UUID
  , organization :: AppConfigOrganization
  , authentication :: AppConfigAuth
  , privacyAndSupport :: AppConfigPrivacyAndSupport
  , dashboard :: AppConfigDashboard
  , lookAndFeel :: AppConfigLookAndFeel
  , registry :: AppConfigRegistry
  , knowledgeModel :: AppConfigKnowledgeModel
  , questionnaire :: AppConfigQuestionnaire
  , submission :: AppConfigSubmission
  , feature :: AppConfigFeature
  , owl :: AppConfigOwl
  , mailConfigUuid :: Maybe U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq AppConfig where
  a == b =
    uuid a == uuid b
      && organization a == organization b
      && authentication a == authentication b
      && privacyAndSupport a == privacyAndSupport b
      && dashboard a == dashboard b
      && lookAndFeel a == lookAndFeel b
      && registry a == registry b
      && questionnaire a == questionnaire b
      && submission a == submission b
      && feature a == feature b
      && owl a == owl b
      && mailConfigUuid a == mailConfigUuid b

data AppConfigOrganization = AppConfigOrganization
  { name :: String
  , description :: String
  , organizationId :: String
  , affiliations :: [String]
  }
  deriving (Generic, Eq, Show)

instance Hashable AppConfigOrganization

data AppConfigAuth = AppConfigAuth
  { defaultRole :: String
  , internal :: AppConfigAuthInternal
  , external :: AppConfigAuthExternal
  }
  deriving (Generic, Eq, Show)

data AppConfigAuthInternal = AppConfigAuthInternal
  { registration :: SimpleFeature
  , twoFactorAuth :: AppConfigAuthInternalTwoFactorAuth
  }
  deriving (Generic, Eq, Show)

data AppConfigAuthInternalTwoFactorAuth = AppConfigAuthInternalTwoFactorAuth
  { enabled :: Bool
  , codeLength :: Int
  , expiration :: Int
  }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternal = AppConfigAuthExternal
  { services :: [AppConfigAuthExternalService]
  }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternalService = AppConfigAuthExternalService
  { aId :: String
  , name :: String
  , url :: String
  , clientId :: String
  , clientSecret :: String
  , parameteres :: [AppConfigAuthExternalServiceParameter]
  , style :: Maybe AppConfigAuthExternalServiceStyle
  }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternalServiceParameter = AppConfigAuthExternalServiceParameter
  { name :: String
  , value :: String
  }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternalServiceStyle = AppConfigAuthExternalServiceStyle
  { icon :: Maybe String
  , background :: Maybe String
  , color :: Maybe String
  }
  deriving (Generic, Eq, Show)

data AppConfigPrivacyAndSupport = AppConfigPrivacyAndSupport
  { privacyUrl :: Maybe String
  , termsOfServiceUrl :: Maybe String
  , supportEmail :: Maybe String
  , supportSiteName :: Maybe String
  , supportSiteUrl :: Maybe String
  , supportSiteIcon :: Maybe String
  }
  deriving (Generic, Eq, Show)

data AppConfigDashboard = AppConfigDashboard
  { dashboardType :: AppConfigDashboardDashboardType
  , welcomeWarning :: Maybe String
  , welcomeInfo :: Maybe String
  }
  deriving (Generic, Eq, Show)

data AppConfigDashboardDashboardType
  = WelcomeDashboardType
  | RoleBasedDashboardType
  deriving (Generic, Eq, Show)

data AppConfigLookAndFeel = AppConfigLookAndFeel
  { appTitle :: Maybe String
  , appTitleShort :: Maybe String
  , customMenuLinks :: [AppConfigLookAndFeelCustomMenuLink]
  , loginInfo :: Maybe String
  , logoUrl :: Maybe String
  , styleUrl :: Maybe String
  , primaryColor :: Maybe String
  , illustrationsColor :: Maybe String
  }
  deriving (Generic, Eq, Show)

data AppConfigLookAndFeelCustomMenuLink = AppConfigLookAndFeelCustomMenuLink
  { icon :: String
  , title :: String
  , url :: String
  , newWindow :: Bool
  }
  deriving (Show, Eq, Generic)

data AppConfigRegistry = AppConfigRegistry
  { enabled :: Bool
  , token :: String
  }
  deriving (Generic, Eq, Show)

data AppConfigKnowledgeModel = AppConfigKnowledgeModel
  { public :: AppConfigKnowledgeModelPublic
  , integrationConfig :: String
  }
  deriving (Generic, Eq, Show)

data AppConfigKnowledgeModelPublic = AppConfigKnowledgeModelPublic
  { enabled :: Bool
  , packages :: [PackagePattern]
  }
  deriving (Generic, Eq, Show)

data AppConfigQuestionnaire = AppConfigQuestionnaire
  { questionnaireVisibility :: AppConfigQuestionnaireVisibility
  , questionnaireSharing :: AppConfigQuestionnaireSharing
  , questionnaireCreation :: QuestionnaireCreation
  , projectTagging :: AppConfigQuestionnaireProjectTagging
  , summaryReport :: SimpleFeature
  , feedback :: AppConfigQuestionnaireFeedback
  }
  deriving (Generic, Eq, Show)

data AppConfigQuestionnaireVisibility = AppConfigQuestionnaireVisibility
  { enabled :: Bool
  , defaultValue :: QuestionnaireVisibility
  }
  deriving (Generic, Eq, Show)

data AppConfigQuestionnaireSharing = AppConfigQuestionnaireSharing
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

data AppConfigQuestionnaireProjectTagging = AppConfigQuestionnaireProjectTagging
  { enabled :: Bool
  , tags :: [String]
  }
  deriving (Generic, Eq, Show)

data AppConfigQuestionnaireFeedback = AppConfigQuestionnaireFeedback
  { enabled :: Bool
  , token :: String
  , owner :: String
  , repo :: String
  }
  deriving (Generic, Eq, Show)

data AppConfigSubmission = AppConfigSubmission
  { enabled :: Bool
  , services :: [AppConfigSubmissionService]
  }
  deriving (Generic, Eq, Show)

data AppConfigSubmissionService = AppConfigSubmissionService
  { sId :: String
  , name :: String
  , description :: String
  , props :: [String]
  , supportedFormats :: [AppConfigSubmissionServiceSupportedFormat]
  , request :: AppConfigSubmissionServiceRequest
  }
  deriving (Generic, Eq, Show)

data AppConfigSubmissionServiceSupportedFormat = AppConfigSubmissionServiceSupportedFormat
  { templateId :: String
  , formatUuid :: U.UUID
  }
  deriving (Generic, Eq, Show)

data AppConfigSubmissionServiceRequest = AppConfigSubmissionServiceRequest
  { method :: String
  , url :: String
  , headers :: M.Map String String
  , multipart :: AppConfigSubmissionServiceRequestMultipart
  }
  deriving (Generic, Eq, Show)

data AppConfigSubmissionServiceRequestMultipart = AppConfigSubmissionServiceRequestMultipart
  { enabled :: Bool
  , fileName :: String
  }
  deriving (Generic, Eq, Show)

data AppConfigFeature = AppConfigFeature
  { clientCustomizationEnabled :: Bool
  , pdfOnlyEnabled :: Bool
  , pdfWatermarkEnabled :: Bool
  }
  deriving (Generic, Eq, Show)

data AppConfigOwl = AppConfigOwl
  { enabled :: Bool
  , name :: String
  , organizationId :: String
  , kmId :: String
  , version :: String
  , previousPackageId :: Maybe String
  , rootElement :: String
  }
  deriving (Generic, Eq, Show)
