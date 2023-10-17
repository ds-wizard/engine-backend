module WizardLib.Public.Model.PersistentCommand.User.CreateOrUpdateUserGroupCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data CreateOrUpdateUserGroupCommand = CreateOrUpdateUserGroupCommand
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , private :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateOrUpdateUserGroupCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateOrUpdateUserGroupCommand where
  toJSON = genericToJSON jsonOptions
