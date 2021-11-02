module Wizard.Model.Config.ServerConfig where

import GHC.Generics
import Shared.Model.Config.Environment
import Shared.Model.Config.ServerConfig

data ServerConfig =
  ServerConfig
    { _serverConfigGeneral :: ServerConfigGeneral
    , _serverConfigDatabase :: ServerConfigDatabase
    , _serverConfigS3 :: ServerConfigS3
    , _serverConfigMessaging :: ServerConfigMessaging
    , _serverConfigJwt :: ServerConfigJwt
    , _serverConfigRoles :: ServerConfigRoles
    , _serverConfigMail :: ServerConfigMail
    , _serverConfigRegistry :: ServerConfigRegistry
    , _serverConfigAnalytics :: ServerConfigAnalytics
    , _serverConfigDocument :: ServerConfigDocument
    , _serverConfigFeedback :: ServerConfigFeedback
    , _serverConfigQuestionnaire :: ServerConfigQuestionnaire
    , _serverConfigLogging :: ServerConfigLogging
    , _serverConfigExperimental :: ServerConfigExperimental
    }
  deriving (Generic, Show)

data ServerConfigGeneral =
  ServerConfigGeneral
    { _serverConfigGeneralEnvironment :: Environment
    , _serverConfigGeneralClientUrl :: String
    , _serverConfigGeneralServerPort :: Int
    , _serverConfigGeneralServiceToken :: String
    , _serverConfigGeneralSecret :: String
    , _serverConfigGeneralIntegrationConfig :: String
    , _serverConfigGeneralTemplateFolder :: String
    , _serverConfigGeneralRemoteLocalizationUrl :: Maybe String
    }
  deriving (Generic, Show)

data ServerConfigMessaging =
  ServerConfigMessaging
    { _serverConfigMessagingEnabled :: Bool
    , _serverConfigMessagingHost :: String
    , _serverConfigMessagingPort :: Integer
    , _serverConfigMessagingUsername :: String
    , _serverConfigMessagingPassword :: String
    , _serverConfigMessagingVhost :: String
    }
  deriving (Generic, Show)

data ServerConfigJwt =
  ServerConfigJwt
    { _serverConfigJwtVersion :: Integer
    , _serverConfigJwtExpiration :: Integer
    }
  deriving (Generic, Show)

data ServerConfigRoles =
  ServerConfigRoles
    { _serverConfigRolesAdmin :: [String]
    , _serverConfigRolesDataSteward :: [String]
    , _serverConfigRolesResearcher :: [String]
    }
  deriving (Generic, Show)

data ServerConfigRegistry =
  ServerConfigRegistry
    { _serverConfigRegistryUrl :: String
    , _serverConfigRegistryClientUrl :: String
    }
  deriving (Generic, Show)

data ServerConfigDocument =
  ServerConfigDocument
    { _serverConfigDocumentClean :: ServerConfigCronWorker
    }
  deriving (Generic, Show)

data ServerConfigFeedback =
  ServerConfigFeedback
    { _serverConfigFeedbackApiUrl :: String
    , _serverConfigFeedbackWebUrl :: String
    , _serverConfigFeedbackSync :: ServerConfigCronWorker
    }
  deriving (Generic, Show)

data ServerConfigQuestionnaire =
  ServerConfigQuestionnaire
    { _serverConfigQuestionnaireClean :: ServerConfigCronWorker
    , _serverConfigQuestionnaireSquash :: ServerConfigCronWorker
    }
  deriving (Generic, Show)

data ServerConfigCronWorker =
  ServerConfigCronWorker
    { _serverConfigCronWorkerEnabled :: Bool
    , _serverConfigCronWorkerCron :: String
    }
  deriving (Generic, Show)
