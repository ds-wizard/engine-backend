module Wizard.Model.PersistentCommand.Mail.SendRegistrationConfirmationMailCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Util.Aeson

data SendRegistrationConfirmationMailCommand = SendRegistrationConfirmationMailCommand
  { email :: String
  , userUuid :: U.UUID
  , userFirstName :: String
  , userLastName :: String
  , userEmail :: String
  , hash :: String
  , clientUrl :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON SendRegistrationConfirmationMailCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SendRegistrationConfirmationMailCommand where
  toJSON = genericToJSON jsonOptions
