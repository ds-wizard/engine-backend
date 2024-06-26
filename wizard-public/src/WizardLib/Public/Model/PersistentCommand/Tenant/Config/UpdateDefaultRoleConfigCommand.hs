module WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateDefaultRoleConfigCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson

data UpdateDefaultRoleConfigCommand = UpdateDefaultRoleConfigCommand
  { defaultRole :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateDefaultRoleConfigCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateDefaultRoleConfigCommand where
  toJSON = genericToJSON jsonOptions
