module Registry.Model.PersistentCommand.Mail.SendResetTokenMailCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson

data SendResetTokenMailCommand = SendResetTokenMailCommand
  { email :: String
  , organizationId :: String
  , organizationName :: String
  , organizationEmail :: String
  , hash :: String
  , clientUrl :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON SendResetTokenMailCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SendResetTokenMailCommand where
  toJSON = genericToJSON jsonOptions
