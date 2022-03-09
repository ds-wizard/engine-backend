module Registry.Model.PersistentCommand.Mail.SendRegistrationCreatedAnalyticsMailCommand where

import Data.Aeson
import GHC.Generics

import Shared.Util.JSON

data SendRegistrationCreatedAnalyticsMailCommand =
  SendRegistrationCreatedAnalyticsMailCommand
    { _sendRegistrationCreatedAnalyticsMailCommandEmail :: String
    , _sendRegistrationCreatedAnalyticsMailCommandOrganizationId :: String
    , _sendRegistrationCreatedAnalyticsMailCommandClientUrl :: String
    }
  deriving (Show, Eq, Generic)

instance FromJSON SendRegistrationCreatedAnalyticsMailCommand where
  parseJSON = simpleParseJSON "_sendRegistrationCreatedAnalyticsMailCommand"

instance ToJSON SendRegistrationCreatedAnalyticsMailCommand where
  toJSON = simpleToJSON "_sendRegistrationCreatedAnalyticsMailCommand"
