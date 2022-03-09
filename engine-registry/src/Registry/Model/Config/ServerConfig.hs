module Registry.Model.Config.ServerConfig where

import GHC.Generics

import Shared.Model.Config.Environment
import Shared.Model.Config.ServerConfig

data ServerConfig =
  ServerConfig
    { _serverConfigGeneral :: ServerConfigGeneral
    , _serverConfigDatabase :: ServerConfigDatabase
    , _serverConfigS3 :: ServerConfigS3
    , _serverConfigAnalytics :: ServerConfigAnalytics
    , _serverConfigSentry :: ServerConfigSentry
    , _serverConfigLogging :: ServerConfigLogging
    , _serverConfigCloud :: ServerConfigCloud
    }
  deriving (Generic, Show)

data ServerConfigGeneral =
  ServerConfigGeneral
    { _serverConfigGeneralEnvironment :: Environment
    , _serverConfigGeneralClientUrl :: String
    , _serverConfigGeneralServerPort :: Int
    , _serverConfigGeneralRemoteLocalizationUrl :: Maybe String
    }
  deriving (Generic, Show)
