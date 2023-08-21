module WizardLib.Public.Model.PersistentCommand.Config.UpdateAppConfigRegistryCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data UpdateAppConfigRegistryCommand = UpdateAppConfigRegistryCommand
  { enabled :: Bool
  , token :: String
  , appUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateAppConfigRegistryCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateAppConfigRegistryCommand where
  toJSON = genericToJSON jsonOptions
