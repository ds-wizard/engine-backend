module WizardLib.Public.Model.PersistentCommand.Tenant.Config.UpdateFeaturesConfigCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.Tenant.Config.TenantConfigJM ()

data UpdateFeaturesConfigCommand = UpdateFeaturesConfigCommand
  { aiAssistantEnabled :: Bool
  , toursEnabled :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON UpdateFeaturesConfigCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateFeaturesConfigCommand where
  toJSON = genericToJSON jsonOptions
