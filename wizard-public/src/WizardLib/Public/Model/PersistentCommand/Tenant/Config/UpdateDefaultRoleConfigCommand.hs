module WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateDefaultRoleConfigCommand where

import Data.Aeson
import qualified Data.UUID as U
import GHC.Generics

import Shared.Common.Util.Aeson

data UpdateDefaultRoleConfigCommand = UpdateDefaultRoleConfigCommand
  { defaultRoleUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateDefaultRoleConfigCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateDefaultRoleConfigCommand where
  toJSON = genericToJSON jsonOptions
