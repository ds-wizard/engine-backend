module Wizard.Model.Config.ServerConfig where

import qualified Crypto.PubKey.RSA as RSA
import GHC.Generics

import Shared.Common.Model.Config.ServerConfig

data ServerConfig = ServerConfig
  { general :: ServerConfigGeneral
  , database :: ServerConfigDatabase
  , s3 :: ServerConfigS3
  , aws :: ServerConfigAws
  , sentry :: ServerConfigSentry
  , jwt :: ServerConfigJwt
  , roles :: ServerConfigRoles
  , actionKey :: ServerConfigActionKey
  , branch :: ServerConfigBranch
  , cache :: ServerConfigCache
  , document :: ServerConfigDocument
  , feedback :: ServerConfigFeedback
  , questionnaire :: ServerConfigQuestionnaire
  , temporaryFile :: ServerConfigTemporaryFile
  , userToken :: ServerConfigUserToken
  , analyticalMails :: ServerConfigAnalyticalMails
  , logging :: ServerConfigLogging
  , cloud :: ServerConfigCloud
  , plan :: ServerConfigPlan
  , persistentCommand :: ServerConfigPersistentCommand
  , signalBridge :: ServerConfigSignalBridge
  , admin :: ServerConfigAdmin
  , registry :: ServerConfigRegistry
  , modules :: ServerConfigModules
  }
  deriving (Generic, Show)

data ServerConfigGeneral = ServerConfigGeneral
  { environment :: String
  , clientUrl :: String
  , serverPort :: Int
  , secret :: String
  , rsaPrivateKey :: RSA.PrivateKey
  , integrationConfig :: String
  }
  deriving (Generic, Show)

data ServerConfigRoles = ServerConfigRoles
  { admin :: [String]
  , dataSteward :: [String]
  , researcher :: [String]
  }
  deriving (Generic, Show)

data ServerConfigActionKey = ServerConfigActionKey
  { clean :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigBranch = ServerConfigBranch
  { squash :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigCache = ServerConfigCache
  { dataExpiration :: Integer
  , websocketExpiration :: Integer
  , purgeExpired :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigDocument = ServerConfigDocument
  { clean :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigFeedback = ServerConfigFeedback
  { apiUrl :: String
  , webUrl :: String
  , sync :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigQuestionnaire = ServerConfigQuestionnaire
  { clean :: ServerConfigCronWorker
  , squash :: ServerConfigCronWorker
  , assigneeNotification :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigTemporaryFile = ServerConfigTemporaryFile
  { clean :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigUserToken = ServerConfigUserToken
  { clean :: ServerConfigCronWorker
  , expire :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigSignalBridge = ServerConfigSignalBridge
  { enabled :: Bool
  , updatePermsArn :: String
  , updateUserGroupArn :: String
  , setQuestionnaireArn :: String
  , logOutAllArn :: String
  }
  deriving (Generic, Show)

data ServerConfigAdmin = ServerConfigAdmin
  { enabled :: Bool
  }
  deriving (Generic, Show)

data ServerConfigRegistry = ServerConfigRegistry
  { url :: String
  , clientUrl :: String
  , sync :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigModules = ServerConfigModules
  { wizard :: ServerConfigModule
  , admin :: ServerConfigModule
  , integrationHub :: ServerConfigModule
  , analytics :: ServerConfigModule
  , guide :: ServerConfigModule
  }
  deriving (Generic, Show)

data ServerConfigModule = ServerConfigModule
  { title :: String
  , description :: String
  , icon :: String
  , url :: Maybe String
  }
  deriving (Generic, Show)
