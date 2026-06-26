module Wizard.Model.Config.ServerConfig where

import qualified Crypto.PubKey.RSA as RSA
import GHC.Generics

import Shared.Common.Model.Config.ServerConfig
import WizardLib.Public.Model.Config.ServerConfig

data ServerConfig = ServerConfig
  { general :: ServerConfigGeneral
  , database :: ServerConfigDatabase
  , s3 :: ServerConfigS3
  , aws :: ServerConfigAws
  , sentry :: ServerConfigSentry
  , userEmailLink :: ServerConfigUserEmailLink
  , cache :: ServerConfigCache
  , document :: ServerConfigDocument
  , externalLink :: ServerConfigExternalLink
  , feedback :: ServerConfigFeedback
  , knowledgeModelEditor :: ServerConfigKnowledgeModelEditor
  , project :: ServerConfigProject
  , temporaryFile :: ServerConfigTemporaryFile
  , userToken :: ServerConfigUserToken
  , analyticalMails :: ServerConfigAnalyticalMails
  , logging :: ServerConfigLogging
  , cloud :: ServerConfigCloud
  , persistentCommand :: ServerConfigPersistentCommand
  , signalBridge :: ServerConfigSignalBridge
  , admin :: ServerConfigAdmin
  , registry :: ServerConfigRegistry
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

data ServerConfigUserEmailLink = ServerConfigUserEmailLink
  { clean :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigCache = ServerConfigCache
  { dataExpiration :: Integer
  , websocketExpiration :: Integer
  , purgeExpired :: ServerConfigCronWorker
  , dataEnabled :: Bool
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

data ServerConfigKnowledgeModelEditor = ServerConfigKnowledgeModelEditor
  { squash :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigProject = ServerConfigProject
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
  , setProjectArn :: String
  , addEventArn :: String
  , addFileArn :: String
  , logOutAllArn :: String
  }
  deriving (Generic, Show)

data ServerConfigAdmin = ServerConfigAdmin
  { enabled :: Bool
  , serverUrl :: String
  }
  deriving (Generic, Show)

data ServerConfigRegistry = ServerConfigRegistry
  { url :: String
  , clientUrl :: String
  , sync :: ServerConfigCronWorker
  }
  deriving (Generic, Show)
