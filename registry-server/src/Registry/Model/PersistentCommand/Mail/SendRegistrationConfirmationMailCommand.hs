module Registry.Model.PersistentCommand.Mail.SendRegistrationConfirmationMailCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson

data SendRegistrationConfirmationMailCommand = SendRegistrationConfirmationMailCommand
  { email :: String
  , organizationId :: String
  , organizationName :: String
  , organizationEmail :: String
  , hash :: String
  , clientUrl :: String
  , callbackUrl :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance FromJSON SendRegistrationConfirmationMailCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SendRegistrationConfirmationMailCommand where
  toJSON = genericToJSON jsonOptions
