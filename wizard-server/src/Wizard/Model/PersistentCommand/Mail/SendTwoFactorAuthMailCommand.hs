module Wizard.Model.PersistentCommand.Mail.SendTwoFactorAuthMailCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data SendTwoFactorAuthMailCommand = SendTwoFactorAuthMailCommand
  { email :: String
  , userUuid :: U.UUID
  , userFirstName :: String
  , userLastName :: String
  , userEmail :: String
  , code :: String
  , clientUrl :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON SendTwoFactorAuthMailCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SendTwoFactorAuthMailCommand where
  toJSON = genericToJSON jsonOptions
