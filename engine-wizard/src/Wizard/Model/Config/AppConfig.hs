module Wizard.Model.Config.AppConfig where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Config.SimpleFeature
import Wizard.Model.User.User

data AppConfig =
  AppConfig
    { _appConfigOrganization :: AppConfigOrganization
    , _appConfigAuthentication :: AppConfigAuth
    , _appConfigPrivacyAndSupport :: AppConfigPrivacyAndSupport
    , _appConfigDashboard :: AppConfigDashboard
    , _appConfigLookAndFeel :: AppConfigLookAndFeel
    , _appConfigKnowledgeModelRegistry :: AppConfigRegistry
    , _appConfigQuestionnaire :: AppConfigQuestionnaire
    , _appConfigTemplate :: AppConfigTemplate
    , _appConfigSubmission :: AppConfigSubmission
    , _appConfigCreatedAt :: UTCTime
    , _appConfigUpdatedAt :: UTCTime
    }
  deriving (Generic, Show)

instance Eq AppConfig where
  a == b =
    _appConfigOrganization a == _appConfigOrganization b &&
    _appConfigAuthentication a == _appConfigAuthentication b &&
    _appConfigPrivacyAndSupport a == _appConfigPrivacyAndSupport b &&
    _appConfigDashboard a == _appConfigDashboard b &&
    _appConfigLookAndFeel a == _appConfigLookAndFeel b &&
    _appConfigKnowledgeModelRegistry a == _appConfigKnowledgeModelRegistry b &&
    _appConfigQuestionnaire a == _appConfigQuestionnaire b && _appConfigSubmission a == _appConfigSubmission b

data AppConfigOrganization =
  AppConfigOrganization
    { _appConfigOrganizationName :: String
    , _appConfigOrganizationDescription :: String
    , _appConfigOrganizationOrganizationId :: String
    , _appConfigOrganizationAffiliations :: [String]
    }
  deriving (Generic, Eq, Show)

data AppConfigAuth =
  AppConfigAuth
    { _appConfigAuthDefaultRole :: Role
    , _appConfigAuthInternal :: AppConfigAuthInternal
    , _appConfigAuthExternal :: AppConfigAuthExternal
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthInternal =
  AppConfigAuthInternal
    { _appConfigAuthInternalRegistration :: SimpleFeature
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternal =
  AppConfigAuthExternal
    { _appConfigAuthExternalServices :: [AppConfigAuthExternalService]
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternalService =
  AppConfigAuthExternalService
    { _appConfigAuthExternalServiceAId :: String
    , _appConfigAuthExternalServiceName :: String
    , _appConfigAuthExternalServiceUrl :: String
    , _appConfigAuthExternalServiceClientId :: String
    , _appConfigAuthExternalServiceClientSecret :: String
    , _appConfigAuthExternalServiceParameteres :: [AppConfigAuthExternalServiceParameter]
    , _appConfigAuthExternalServiceStyle :: Maybe AppConfigAuthExternalServiceStyle
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternalServiceParameter =
  AppConfigAuthExternalServiceParameter
    { _appConfigAuthExternalServiceParameterName :: String
    , _appConfigAuthExternalServiceParameterValue :: String
    }
  deriving (Generic, Eq, Show)

data AppConfigAuthExternalServiceStyle =
  AppConfigAuthExternalServiceStyle
    { _appConfigAuthExternalServiceStyleIcon :: Maybe String
    , _appConfigAuthExternalServiceStyleBackground :: Maybe String
    , _appConfigAuthExternalServiceStyleColor :: Maybe String
    }
  deriving (Generic, Eq, Show)

data AppConfigPrivacyAndSupport =
  AppConfigPrivacyAndSupport
    { _appConfigPrivacyAndSupportPrivacyUrl :: Maybe String
    , _appConfigPrivacyAndSupportSupportEmail :: Maybe String
    , _appConfigPrivacyAndSupportSupportRepositoryName :: Maybe String
    , _appConfigPrivacyAndSupportSupportRepositoryUrl :: Maybe String
    }
  deriving (Generic, Eq, Show)

data AppConfigDashboard =
  AppConfigDashboard
    { _appConfigDashboardWidgets :: Maybe AppConfigDashboardWidgets
    , _appConfigDashboardWelcomeWarning :: Maybe String
    , _appConfigDashboardWelcomeInfo :: Maybe String
    }
  deriving (Generic, Eq, Show)

data AppConfigDashboardWidgets =
  AppConfigDashboardWidgets
    { _appConfigDashboardWidgetsAdmin :: [String]
    , _appConfigDashboardWidgetsDataSteward :: [String]
    , _appConfigDashboardWidgetsResearcher :: [String]
    }
  deriving (Generic, Eq, Show)

data AppConfigLookAndFeel =
  AppConfigLookAndFeel
    { _appConfigLookAndFeelAppTitle :: Maybe String
    , _appConfigLookAndFeelAppTitleShort :: Maybe String
    , _appConfigLookAndFeelCustomMenuLinks :: [AppConfigLookAndFeelCustomMenuLink]
    , _appConfigLookAndFeelLoginInfo :: Maybe String
    }
  deriving (Generic, Eq, Show)

data AppConfigLookAndFeelCustomMenuLink =
  AppConfigLookAndFeelCustomMenuLink
    { _appConfigLookAndFeelCustomMenuLinkIcon :: String
    , _appConfigLookAndFeelCustomMenuLinkTitle :: String
    , _appConfigLookAndFeelCustomMenuLinkUrl :: String
    , _appConfigLookAndFeelCustomMenuLinkNewWindow :: Bool
    }
  deriving (Show, Eq, Generic)

data AppConfigRegistry =
  AppConfigRegistry
    { _appConfigRegistryEnabled :: Bool
    , _appConfigRegistryToken :: String
    }
  deriving (Generic, Eq, Show)

data AppConfigQuestionnaire =
  AppConfigQuestionnaire
    { _appConfigQuestionnaireQuestionnaireAccessibility :: SimpleFeature
    , _appConfigQuestionnaireLevels :: SimpleFeature
    , _appConfigQuestionnaireFeedback :: AppConfigQuestionnaireFeedback
    }
  deriving (Generic, Eq, Show)

data AppConfigQuestionnaireFeedback =
  AppConfigQuestionnaireFeedback
    { _appConfigQuestionnaireFeedbackEnabled :: Bool
    , _appConfigQuestionnaireFeedbackToken :: String
    , _appConfigQuestionnaireFeedbackOwner :: String
    , _appConfigQuestionnaireFeedbackRepo :: String
    }
  deriving (Generic, Eq, Show)

data AppConfigTemplate =
  AppConfigTemplate
    { _appConfigTemplateRecommendedTemplateUuid :: Maybe U.UUID
    }
  deriving (Generic, Eq, Show)

data AppConfigSubmission =
  AppConfigSubmission
    { _appConfigSubmissionEnabled :: Bool
    , _appConfigSubmissionServices :: [AppConfigSubmissionService]
    }
  deriving (Generic, Eq, Show)

data AppConfigSubmissionService =
  AppConfigSubmissionService
    { _appConfigSubmissionServiceSId :: String
    , _appConfigSubmissionServiceName :: String
    , _appConfigSubmissionServiceDescription :: String
    , _appConfigSubmissionServiceProps :: [String]
    , _appConfigSubmissionServiceSupportedFormats :: [AppConfigSubmissionServiceSupportedFormat]
    , _appConfigSubmissionServiceRequest :: AppConfigSubmissionServiceRequest
    }
  deriving (Generic, Eq, Show)

data AppConfigSubmissionServiceSupportedFormat =
  AppConfigSubmissionServiceSupportedFormat
    { _appConfigSubmissionServiceSupportedFormatTemplateUuid :: U.UUID
    , _appConfigSubmissionServiceSupportedFormatFormatUuid :: U.UUID
    }
  deriving (Generic, Eq, Show)

data AppConfigSubmissionServiceRequest =
  AppConfigSubmissionServiceRequest
    { _appConfigSubmissionServiceRequestMethod :: String
    , _appConfigSubmissionServiceRequestUrl :: String
    , _appConfigSubmissionServiceRequestHeaders :: M.Map String String
    , _appConfigSubmissionServiceRequestMultipart :: AppConfigSubmissionServiceRequestMultipart
    }
  deriving (Generic, Eq, Show)

data AppConfigSubmissionServiceRequestMultipart =
  AppConfigSubmissionServiceRequestMultipart
    { _appConfigSubmissionServiceRequestMultipartEnabled :: Bool
    , _appConfigSubmissionServiceRequestMultipartFileName :: String
    }
  deriving (Generic, Eq, Show)
