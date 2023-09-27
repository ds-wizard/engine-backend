module Wizard.Api.Resource.Tenant.TenantCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Tenant.TenantCreateDTO

instance FromJSON TenantCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantCreateDTO where
  toJSON = genericToJSON jsonOptions
