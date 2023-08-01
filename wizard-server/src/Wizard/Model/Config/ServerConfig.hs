module Wizard.Model.Config.ServerConfig where

import qualified Crypto.PubKey.RSA as RSA
import GHC.Generics

import Shared.Common.Model.Config.Environment
import Shared.Common.Model.Config.ServerConfig

data ServerConfig = ServerConfig
  { general :: ServerConfigGeneral
  , database :: ServerConfigDatabase
  , s3 :: ServerConfigS3
  , jwt :: ServerConfigJwt
  , roles :: ServerConfigRoles
  , registry :: ServerConfigRegistry
  , analytics :: ServerConfigAnalytics
  , sentry :: ServerConfigSentry
  , actionKey :: ServerConfigActionKey
  , branch :: ServerConfigBranch
  , cache :: ServerConfigCache
  , document :: ServerConfigDocument
  , feedback :: ServerConfigFeedback
  , persistentCommand :: ServerConfigPersistentCommand
  , plan :: ServerConfigPlan
  , questionnaire :: ServerConfigQuestionnaire
  , temporaryFile :: ServerConfigTemporaryFile
  , userToken :: ServerConfigUserToken
  , logging :: ServerConfigLogging
  , cloud :: ServerConfigCloud
  , admin :: ServerConfigAdmin
  }
  deriving (Generic, Show)

data ServerConfigGeneral = ServerConfigGeneral
  { environment :: Environment
  , clientUrl :: String
  , serverPort :: Int
  , secret :: String
  , rsaPrivateKey :: RSA.PrivateKey
  , integrationConfig :: String
  , clientStyleBuilderUrl :: String
  }
  deriving (Generic, Show)

data ServerConfigJwt = ServerConfigJwt
  { expiration :: Integer
  }
  deriving (Generic, Show)

data ServerConfigRoles = ServerConfigRoles
  { admin :: [String]
  , dataSteward :: [String]
  , researcher :: [String]
  }
  deriving (Generic, Show)

data ServerConfigRegistry = ServerConfigRegistry
  { url :: String
  , clientUrl :: String
  , sync :: ServerConfigCronWorker
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

data ServerConfigPlan = ServerConfigPlan
  { recomputeJob :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigQuestionnaire = ServerConfigQuestionnaire
  { clean :: ServerConfigCronWorker
  , recomputeIndication :: ServerConfigCronWorker
  , squash :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigTemporaryFile = ServerConfigTemporaryFile
  { clean :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigUserToken = ServerConfigUserToken
  { clean :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigAdmin = ServerConfigAdmin
  { enabled :: Bool
  , url :: String
  }
  deriving (Generic, Show)
