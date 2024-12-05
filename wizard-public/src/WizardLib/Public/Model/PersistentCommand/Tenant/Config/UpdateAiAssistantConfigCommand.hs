module WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateAiAssistantConfigCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigJM ()

data UpdateAiAssistantConfigCommand = UpdateAiAssistantConfigCommand
  { enabled :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateAiAssistantConfigCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateAiAssistantConfigCommand where
  toJSON = genericToJSON jsonOptions
