module Registry.Model.PersistentCommand.Mail.SendRegistrationConfirmationMailCommand where

import Data.Aeson
import GHC.Generics

import Shared.Util.JSON

data SendRegistrationConfirmationMailCommand =
  SendRegistrationConfirmationMailCommand
    { _sendRegistrationConfirmationMailCommandEmail :: String
    , _sendRegistrationConfirmationMailCommandOrganizationId :: String
    , _sendRegistrationConfirmationMailCommandOrganizationName :: String
    , _sendRegistrationConfirmationMailCommandOrganizationEmail :: String
    , _sendRegistrationConfirmationMailCommandHash :: String
    , _sendRegistrationConfirmationMailCommandClientUrl :: String
    , _sendRegistrationConfirmationMailCommandCallbackUrl :: Maybe String
    }
  deriving (Show, Eq, Generic)

instance FromJSON SendRegistrationConfirmationMailCommand where
  parseJSON = simpleParseJSON "_sendRegistrationConfirmationMailCommand"

instance ToJSON SendRegistrationConfirmationMailCommand where
  toJSON = simpleToJSON "_sendRegistrationConfirmationMailCommand"
