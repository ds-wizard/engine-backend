module WizardLib.Public.Model.PersistentCommand.User.DeleteUserCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data DeleteUserCommand = DeleteUserCommand
  { uuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON DeleteUserCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DeleteUserCommand where
  toJSON = genericToJSON jsonOptions
