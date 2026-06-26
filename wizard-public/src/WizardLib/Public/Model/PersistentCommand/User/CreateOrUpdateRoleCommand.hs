module WizardLib.Public.Model.PersistentCommand.User.CreateOrUpdateRoleCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data CreateOrUpdateRoleCommand = CreateOrUpdateRoleCommand
  { uuid :: U.UUID
  , name :: String
  , permissions :: [String]
  , isAdmin :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateOrUpdateRoleCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateOrUpdateRoleCommand where
  toJSON = genericToJSON jsonOptions
