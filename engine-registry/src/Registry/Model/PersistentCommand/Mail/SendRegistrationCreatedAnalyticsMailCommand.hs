module Registry.Model.PersistentCommand.Mail.SendRegistrationCreatedAnalyticsMailCommand where

import Data.Aeson
import GHC.Generics

import Shared.Util.Aeson

data SendRegistrationCreatedAnalyticsMailCommand = SendRegistrationCreatedAnalyticsMailCommand
  { email :: String
  , organizationId :: String
  , organizationName :: String
  , organizationEmail :: String
  , clientUrl :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON SendRegistrationCreatedAnalyticsMailCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SendRegistrationCreatedAnalyticsMailCommand where
  toJSON = genericToJSON jsonOptions
