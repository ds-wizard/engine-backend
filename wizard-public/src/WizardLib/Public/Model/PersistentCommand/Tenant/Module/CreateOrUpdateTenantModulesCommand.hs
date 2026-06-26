module WizardLib.Public.Model.PersistentCommand.Tenant.Module.CreateOrUpdateTenantModulesCommand where

import Data.Aeson
import GHC.Generics

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.Tenant.Module.TenantModuleJM ()
import WizardLib.Public.Model.Tenant.Module.TenantModule

data CreateOrUpdateTenantModulesCommand = CreateOrUpdateTenantModulesCommand
  { modules :: [TenantModule]
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateOrUpdateTenantModulesCommand where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CreateOrUpdateTenantModulesCommand where
  toJSON = genericToJSON jsonOptions
