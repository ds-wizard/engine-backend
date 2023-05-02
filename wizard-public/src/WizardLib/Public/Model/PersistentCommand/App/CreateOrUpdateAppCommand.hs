module WizardLib.Public.Model.PersistentCommand.App.CreateOrUpdateAppCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data CreateOrUpdateAppCommand = CreateOrUpdateAppCommand
  { uuid :: U.UUID
  , appId :: String
  , name :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateOrUpdateAppCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateOrUpdateAppCommand where
  toJSON = genericToJSON jsonOptions
