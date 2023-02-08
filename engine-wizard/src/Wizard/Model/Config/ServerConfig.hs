module Wizard.Model.Config.ServerConfig where

import GHC.Generics
import Shared.Model.Config.Environment
import Shared.Model.Config.ServerConfig

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
  , userToken :: ServerConfigUserToken
  , logging :: ServerConfigLogging
  , cloud :: ServerConfigCloud
  }
  deriving (Generic, Show)

data ServerConfigGeneral = ServerConfigGeneral
  { environment :: Environment
  , clientUrl :: String
  , serverPort :: Int
  , secret :: String
  , integrationConfig :: String
  , clientStyleBuilderUrl :: String
  }
  deriving (Generic, Show)

data ServerConfigJwt = ServerConfigJwt
  { version :: Integer
  , expiration :: Integer
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

data ServerConfigPersistentCommand = ServerConfigPersistentCommand
  { listenerJob :: ServerConfigPersistentCommandListenerJob
  , retryJob :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigPersistentCommandListenerJob = ServerConfigPersistentCommandListenerJob
  { enabled :: Bool
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

data ServerConfigUserToken = ServerConfigUserToken
  { clean :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigCronWorker = ServerConfigCronWorker
  { enabled :: Bool
  , cron :: String
  }
  deriving (Generic, Show)
