module Registry.Model.PersistentCommand.Mail.SendResetTokenMailCommand where

import Data.Aeson
import GHC.Generics

import Shared.Util.JSON

data SendResetTokenMailCommand =
  SendResetTokenMailCommand
    { _sendResetTokenMailCommandEmail :: String
    , _sendResetTokenMailCommandOrganizationId :: String
    , _sendResetTokenMailCommandHash :: String
    , _sendResetTokenMailCommandClientUrl :: String
    }
  deriving (Show, Eq, Generic)

instance FromJSON SendResetTokenMailCommand where
  parseJSON = simpleParseJSON "_sendResetTokenMailCommand"

instance ToJSON SendResetTokenMailCommand where
  toJSON = simpleToJSON "_sendResetTokenMailCommand"
