module Model.Config.AppConfigJM where

import Data.Aeson

import Model.Config.AppConfig
import Model.Config.EnvironmentJM ()
import Util.JSON (simpleParseJSON)

instance FromJSON AppConfig where
  parseJSON = simpleParseJSON "_appConfig"

instance FromJSON AppConfigGeneral where
  parseJSON = simpleParseJSON "_appConfigGeneral"

instance FromJSON AppConfigClient where
  parseJSON = simpleParseJSON "_appConfigClient"

instance FromJSON AppConfigClientDashboard where
  parseJSON = simpleParseJSON "_appConfigClientDashboard"

instance FromJSON AppConfigDatabase where
  parseJSON = simpleParseJSON "_appConfigDatabase"

instance FromJSON AppConfigMessaging where
  parseJSON = simpleParseJSON "_appConfigMessaging"

instance FromJSON AppConfigJwt where
  parseJSON = simpleParseJSON "_appConfigJwt"

instance FromJSON AppConfigRoles where
  parseJSON = simpleParseJSON "_appConfigRoles"

instance FromJSON AppConfigMail where
  parseJSON = simpleParseJSON "_appConfigMail"

instance FromJSON AppConfigAnalytics where
  parseJSON = simpleParseJSON "_appConfigAnalytics"

instance FromJSON AppConfigFeedback where
  parseJSON = simpleParseJSON "_appConfigFeedback"
