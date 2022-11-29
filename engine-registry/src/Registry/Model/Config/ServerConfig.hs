module Registry.Model.Config.ServerConfig where

import GHC.Generics

import Shared.Model.Config.Environment
import Shared.Model.Config.ServerConfig

data ServerConfig = ServerConfig
  { general :: ServerConfigGeneral
  , database :: ServerConfigDatabase
  , s3 :: ServerConfigS3
  , analytics :: ServerConfigAnalytics
  , sentry :: ServerConfigSentry
  , logging :: ServerConfigLogging
  , cloud :: ServerConfigCloud
  }
  deriving (Generic, Show)

data ServerConfigGeneral = ServerConfigGeneral
  { environment :: Environment
  , clientUrl :: String
  , serverPort :: Int
  , remoteLocalizationUrl :: Maybe String
  }
  deriving (Generic, Show)
