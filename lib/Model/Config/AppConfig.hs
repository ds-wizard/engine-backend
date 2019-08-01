module Model.Config.AppConfig where

import GHC.Generics
import Model.Config.Environment
import Model.User.User

data AppConfig = AppConfig
  { _appConfigGeneral :: AppConfigGeneral
  , _appConfigClient :: AppConfigClient
  , _appConfigDatabase :: AppConfigDatabase
  , _appConfigMessaging :: AppConfigMessaging
  , _appConfigJwt :: AppConfigJwt
  , _appConfigRoles :: AppConfigRoles
  , _appConfigMail :: AppConfigMail
  , _appConfigRegistry :: AppConfigRegistry
  , _appConfigAnalytics :: AppConfigAnalytics
  , _appConfigFeedback :: AppConfigFeedback
  } deriving (Generic, Show)

data AppConfigGeneral = AppConfigGeneral
  { _appConfigGeneralEnvironment :: Environment
  , _appConfigGeneralClientUrl :: String
  , _appConfigGeneralServerPort :: Int
  , _appConfigGeneralServiceToken :: String
  , _appConfigGeneralIntegrationConfig :: String
  , _appConfigGeneralRegistrationEnabled :: Bool
  , _appConfigGeneralPublicQuestionnaireEnabled :: Bool
  , _appConfigGeneralLevelsEnabled :: Bool
  , _appConfigGeneralItemTitleEnabled :: Bool
  , _appConfigGeneralQuestionnaireAccessibilityEnabled :: Bool
  } deriving (Generic, Show)

data AppConfigClient = AppConfigClient
  { _appConfigClientPrivacyUrl :: String
  , _appConfigClientAppTitle :: Maybe String
  , _appConfigClientAppTitleShort :: Maybe String
  , _appConfigClientWelcomeWarning :: Maybe String
  , _appConfigClientWelcomeInfo :: Maybe String
  , _appConfigClientDashboard :: Maybe AppConfigClientDashboard
  , _appConfigClientCustomMenuLinks :: [AppConfigClientCustomMenuLink]
  } deriving (Generic, Show)

data AppConfigClientDashboard = AppConfigClientDashboard
  { _appConfigClientDashboardAdmin :: [String]
  , _appConfigClientDashboardDataSteward :: [String]
  , _appConfigClientDashboardResearcher :: [String]
  } deriving (Generic, Show)

data AppConfigClientCustomMenuLink = AppConfigClientCustomMenuLink
  { _appConfigClientCustomMenuLinkIcon :: String
  , _appConfigClientCustomMenuLinkTitle :: String
  , _appConfigClientCustomMenuLinkUrl :: String
  , _appConfigClientCustomMenuLinkNewWindow :: Bool
  } deriving (Show, Eq, Generic)

data AppConfigDatabase = AppConfigDatabase
  { _appConfigDatabaseHost :: String
  , _appConfigDatabaseDatabaseName :: String
  , _appConfigDatabasePort :: Integer
  , _appConfigDatabaseAuthEnabled :: Bool
  , _appConfigDatabaseUsername :: String
  , _appConfigDatabasePassword :: String
  } deriving (Generic, Show)

data AppConfigMessaging = AppConfigMessaging
  { _appConfigMessagingEnabled :: Bool
  , _appConfigMessagingHost :: String
  , _appConfigMessagingPort :: Integer
  , _appConfigMessagingUsername :: String
  , _appConfigMessagingPassword :: String
  } deriving (Generic, Show)

data AppConfigJwt = AppConfigJwt
  { _appConfigJwtSecret :: String
  , _appConfigJwtVersion :: Integer
  , _appConfigJwtExpiration :: Integer
  } deriving (Generic, Show)

data AppConfigRoles = AppConfigRoles
  { _appConfigRolesDefaultRole :: Role
  , _appConfigRolesAdmin :: [Permission]
  , _appConfigRolesDataSteward :: [Permission]
  , _appConfigRolesResearcher :: [Permission]
  } deriving (Generic, Show)

data AppConfigMail = AppConfigMail
  { _appConfigMailEnabled :: Bool
  , _appConfigMailName :: String
  , _appConfigMailEmail :: String
  , _appConfigMailHost :: String
  , _appConfigMailPort :: Int
  , _appConfigMailSsl :: Bool
  , _appConfigMailAuthEnabled :: Bool
  , _appConfigMailUsername :: String
  , _appConfigMailPassword :: String
  } deriving (Generic, Show)

data AppConfigRegistry = AppConfigRegistry
  { _appConfigRegistryEnabled :: Bool
  , _appConfigRegistryUrl :: String
  , _appConfigRegistryToken :: String
  , _appConfigRegistryClientUrl :: String
  } deriving (Generic, Show)

data AppConfigAnalytics = AppConfigAnalytics
  { _appConfigAnalyticsEnabled :: Bool
  , _appConfigAnalyticsEmail :: String
  } deriving (Generic, Show)

data AppConfigFeedback = AppConfigFeedback
  { _appConfigFeedbackEnabled :: Bool
  , _appConfigFeedbackToken :: String
  , _appConfigFeedbackOwner :: String
  , _appConfigFeedbackRepo :: String
  , _appConfigFeedbackIssueUrl :: String
  } deriving (Generic, Show)
