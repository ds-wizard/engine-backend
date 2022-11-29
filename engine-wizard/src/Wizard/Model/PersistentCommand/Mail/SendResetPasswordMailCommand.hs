module Wizard.Model.PersistentCommand.Mail.SendResetPasswordMailCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.Aeson

data SendResetPasswordMailCommand = SendResetPasswordMailCommand
  { email :: String
  , userUuid :: U.UUID
  , userFirstName :: String
  , userLastName :: String
  , userEmail :: String
  , hash :: String
  , clientUrl :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON SendResetPasswordMailCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SendResetPasswordMailCommand where
  toJSON = genericToJSON jsonOptions
