module WizardLib.Public.Api.Resource.Tenant.Module.TenantModuleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Model.Tenant.Module.TenantModule

instance FromJSON TenantModule where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantModule where
  toJSON = genericToJSON jsonOptions
