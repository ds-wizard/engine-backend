module WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateRegistryConfigCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data UpdateRegistryConfigCommand = UpdateRegistryConfigCommand
  { enabled :: Bool
  , token :: String
  , tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateRegistryConfigCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateRegistryConfigCommand where
  toJSON = genericToJSON jsonOptions
