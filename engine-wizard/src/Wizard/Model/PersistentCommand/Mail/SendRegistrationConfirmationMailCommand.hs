module Wizard.Model.PersistentCommand.Mail.SendRegistrationConfirmationMailCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.JSON

data SendRegistrationConfirmationMailCommand =
  SendRegistrationConfirmationMailCommand
    { _sendRegistrationConfirmationMailCommandEmail :: String
    , _sendRegistrationConfirmationMailCommandUserUuid :: U.UUID
    , _sendRegistrationConfirmationMailCommandUserFirstName :: String
    , _sendRegistrationConfirmationMailCommandUserLastName :: String
    , _sendRegistrationConfirmationMailCommandUserEmail :: String
    , _sendRegistrationConfirmationMailCommandHash :: String
    , _sendRegistrationConfirmationMailCommandClientUrl :: String
    }
  deriving (Show, Eq, Generic)

instance FromJSON SendRegistrationConfirmationMailCommand where
  parseJSON = simpleParseJSON "_sendRegistrationConfirmationMailCommand"

instance ToJSON SendRegistrationConfirmationMailCommand where
  toJSON = simpleToJSON "_sendRegistrationConfirmationMailCommand"
