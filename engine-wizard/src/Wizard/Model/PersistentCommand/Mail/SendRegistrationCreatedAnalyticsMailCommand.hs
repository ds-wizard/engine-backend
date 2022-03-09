module Wizard.Model.PersistentCommand.Mail.SendRegistrationCreatedAnalyticsMailCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.JSON

data SendRegistrationCreatedAnalyticsMailCommand =
  SendRegistrationCreatedAnalyticsMailCommand
    { _sendRegistrationCreatedAnalyticsMailCommandEmail :: String
    , _sendRegistrationCreatedAnalyticsMailCommandUserUuid :: U.UUID
    , _sendRegistrationCreatedAnalyticsMailCommandUserFirstName :: String
    , _sendRegistrationCreatedAnalyticsMailCommandUserLastName :: String
    , _sendRegistrationCreatedAnalyticsMailCommandUserEmail :: String
    , _sendRegistrationCreatedAnalyticsMailCommandClientUrl :: String
    }
  deriving (Show, Eq, Generic)

instance FromJSON SendRegistrationCreatedAnalyticsMailCommand where
  parseJSON = simpleParseJSON "_sendRegistrationCreatedAnalyticsMailCommand"

instance ToJSON SendRegistrationCreatedAnalyticsMailCommand where
  toJSON = simpleToJSON "_sendRegistrationCreatedAnalyticsMailCommand"
