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
  } deriving (Generic, Show)

data AppConfigClient = AppConfigClient
  { _appConfigClientAppTitle :: Maybe String
  , _appConfigClientAppTitleShort :: Maybe String
  , _appConfigClientWelcomeWarning :: Maybe String
  , _appConfigClientWelcomeInfo :: Maybe String
  } deriving (Generic, Show)

data AppConfigDatabase = AppConfigDatabase
  { _appConfigDatabaseHost :: String
  , _appConfigDatabaseDatabaseName :: String
  , _appConfigDatabasePort :: Integer
  , _appConfigDatabaseAuthEnabled :: Bool
  , _appConfigDatabaseUsername :: Maybe String
  , _appConfigDatabasePassword :: Maybe String
  } deriving (Generic, Show)

data AppConfigMessaging = AppConfigMessaging
  { _appConfigMessagingEnabled :: Bool
  , _appConfigMessagingHost :: Maybe String
  , _appConfigMessagingPort :: Maybe Integer
  , _appConfigMessagingUsername :: Maybe String
  , _appConfigMessagingPassword :: Maybe String
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
  , _appConfigMailName :: Maybe String
  , _appConfigMailEmail :: Maybe String
  , _appConfigMailHost :: Maybe String
  , _appConfigMailPort :: Maybe Int
  , _appConfigMailSsl :: Maybe Bool
  , _appConfigMailUsername :: Maybe String
  , _appConfigMailPassword :: Maybe String
  } deriving (Generic, Show)

data AppConfigAnalytics = AppConfigAnalytics
  { _appConfigAnalyticsEnabled :: Bool
  , _appConfigAnalyticsEmail :: Maybe String
  } deriving (Generic, Show)

data AppConfigFeedback = AppConfigFeedback
  { _appConfigFeedbackEnabled :: Bool
  , _appConfigFeedbackToken :: Maybe String
  , _appConfigFeedbackOwner :: Maybe String
  , _appConfigFeedbackRepo :: Maybe String
  , _appConfigFeedbackIssueUrl :: Maybe String
  } deriving (Generic, Show)
