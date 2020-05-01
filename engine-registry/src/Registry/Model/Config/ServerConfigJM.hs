module Registry.Model.Config.ServerConfigJM where

import Data.Aeson

import Registry.Model.Config.ServerConfig
import Shared.Model.Config.EnvironmentJM ()
import Shared.Util.JSON

instance FromJSON ServerConfig where
  parseJSON = simpleParseJSON "_serverConfig"

instance FromJSON ServerConfigGeneral where
  parseJSON = simpleParseJSON "_serverConfigGeneral"

instance FromJSON ServerConfigDatabase where
  parseJSON = simpleParseJSON "_serverConfigDatabase"

instance FromJSON ServerConfigMail where
  parseJSON = simpleParseJSON "_serverConfigMail"

instance FromJSON ServerConfigAnalytics where
  parseJSON = simpleParseJSON "_serverConfigAnalytics"
