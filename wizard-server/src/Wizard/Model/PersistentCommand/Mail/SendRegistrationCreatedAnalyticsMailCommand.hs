module Wizard.Model.PersistentCommand.Mail.SendRegistrationCreatedAnalyticsMailCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data SendRegistrationCreatedAnalyticsMailCommand = SendRegistrationCreatedAnalyticsMailCommand
  { email :: String
  , userUuid :: U.UUID
  , userFirstName :: String
  , userLastName :: String
  , userEmail :: String
  , clientUrl :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON SendRegistrationCreatedAnalyticsMailCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SendRegistrationCreatedAnalyticsMailCommand where
  toJSON = genericToJSON jsonOptions
