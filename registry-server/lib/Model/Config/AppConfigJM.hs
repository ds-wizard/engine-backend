module Model.Config.AppConfigJM where

import Data.Aeson

import Model.Config.AppConfig
import Model.Config.EnvironmentJM ()
import Util.JSON (simpleParseJSON)

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
