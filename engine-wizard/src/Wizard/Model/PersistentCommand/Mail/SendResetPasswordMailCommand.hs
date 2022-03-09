module Wizard.Model.PersistentCommand.Mail.SendResetPasswordMailCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.JSON

data SendResetPasswordMailCommand =
  SendResetPasswordMailCommand
    { _sendResetPasswordMailCommandEmail :: String
    , _sendResetPasswordMailCommandUserUuid :: U.UUID
    , _sendResetPasswordMailCommandHash :: String
    , _sendResetPasswordMailCommandClientUrl :: String
    }
  deriving (Show, Eq, Generic)

instance FromJSON SendResetPasswordMailCommand where
  parseJSON = simpleParseJSON "_sendResetPasswordMailCommand"

instance ToJSON SendResetPasswordMailCommand where
  toJSON = simpleToJSON "_sendResetPasswordMailCommand"
