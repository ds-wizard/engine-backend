module Shared.Common.Model.Config.ServerConfig where

import Control.Monad.Logger (LogLevel (..))
import GHC.Generics

data ServerConfigDatabase = ServerConfigDatabase
  { connectionString :: String
  , stripeSize :: Int
  , connectionTimeout :: Int
  , maxConnections :: Int
  }
  deriving (Generic, Show)

data ServerConfigS3 = ServerConfigS3
  { url :: String
  , username :: String
  , password :: String
  , bucket :: String
  , region :: Maybe String
  }
  deriving (Generic, Show)

data ServerConfigAnalytics = ServerConfigAnalytics
  { enabled :: Bool
  , email :: String
  }
  deriving (Generic, Show)

data ServerConfigSentry = ServerConfigSentry
  { enabled :: Bool
  , dsn :: String
  }
  deriving (Generic, Show)

data ServerConfigLogging = ServerConfigLogging
  { level :: LogLevel
  , httpClientDebug :: Bool
  , websocketDebug :: Bool
  }
  deriving (Generic, Show)

data ServerConfigCloud = ServerConfigCloud
  { enabled :: Bool
  , domain :: Maybe String
  , publicRegistrationEnabled :: Bool
  }
  deriving (Generic, Show)

data ServerConfigCronWorker = ServerConfigCronWorker
  { enabled :: Bool
  , cron :: String
  }
  deriving (Generic, Show)

data ServerConfigPersistentCommand = ServerConfigPersistentCommand
  { lambdaFunctions :: [ServerConfigPersistentCommandLambda]
  , listenerJob :: ServerConfigPersistentCommandListenerJob
  , retryJob :: ServerConfigCronWorker
  , retryLambdaJob :: ServerConfigCronWorker
  }
  deriving (Generic, Show)

data ServerConfigPersistentCommandListenerJob = ServerConfigPersistentCommandListenerJob
  { enabled :: Bool
  }
  deriving (Generic, Show)

data ServerConfigPersistentCommandLambda = ServerConfigPersistentCommandLambda
  { component :: String
  , functionArn :: String
  }
  deriving (Generic, Show)

data ServerConfigAws = ServerConfigAws
  { awsAccessKeyId :: String
  , awsSecretAccessKey :: String
  , awsRegion :: String
  }
  deriving (Generic, Show)
