module Wizard.Api.Resource.Tenant.TenantJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Model.Tenant.Tenant

instance FromJSON Tenant where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Tenant where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantState where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantState where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantDTO where
  toJSON = genericToJSON jsonOptions
