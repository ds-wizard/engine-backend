module Registry.Model.Config.ServerConfig where

import GHC.Generics

import Shared.Common.Model.Config.Environment
import Shared.Common.Model.Config.ServerConfig

data ServerConfig = ServerConfig
  { general :: ServerConfigGeneral
  , database :: ServerConfigDatabase
  , s3 :: ServerConfigS3
  , analyticalMails :: ServerConfigAnalyticalMails
  , sentry :: ServerConfigSentry
  , logging :: ServerConfigLogging
  , persistentCommand :: ServerConfigPersistentCommand
  , cloud :: ServerConfigCloud
  }
  deriving (Generic, Show)

data ServerConfigGeneral = ServerConfigGeneral
  { environment :: Environment
  , clientUrl :: String
  , serverPort :: Int
  , publicRegistrationEnabled :: Bool
  }
  deriving (Generic, Show)
