module WizardLib.Public.Model.PersistentCommand.User.DeleteRoleCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data DeleteRoleCommand = DeleteRoleCommand
  { uuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON DeleteRoleCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DeleteRoleCommand where
  toJSON = genericToJSON jsonOptions
