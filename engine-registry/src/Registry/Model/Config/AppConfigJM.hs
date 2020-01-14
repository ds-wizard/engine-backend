module Registry.Model.Config.AppConfigJM where

import Data.Aeson

import Registry.Model.Config.AppConfig
import Registry.Util.JSON (simpleParseJSON)
import Shared.Model.Config.EnvironmentJM ()

instance FromJSON AppConfig where
  parseJSON = simpleParseJSON "_appConfig"

instance FromJSON AppConfigGeneral where
  parseJSON = simpleParseJSON "_appConfigGeneral"

instance FromJSON AppConfigDatabase where
  parseJSON = simpleParseJSON "_appConfigDatabase"

instance FromJSON AppConfigMail where
  parseJSON = simpleParseJSON "_appConfigMail"

instance FromJSON AppConfigAnalytics where
  parseJSON = simpleParseJSON "_appConfigAnalytics"
