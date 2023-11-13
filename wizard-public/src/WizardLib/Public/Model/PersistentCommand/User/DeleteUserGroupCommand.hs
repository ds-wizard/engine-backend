module WizardLib.Public.Model.PersistentCommand.User.DeleteUserGroupCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data DeleteUserGroupCommand = DeleteUserGroupCommand
  { uuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON DeleteUserGroupCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DeleteUserGroupCommand where
  toJSON = genericToJSON jsonOptions
