module Wizard.Api.Resource.Tenant.TenantChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Tenant.TenantChangeDTO

instance FromJSON TenantChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantChangeDTO where
  toJSON = genericToJSON jsonOptions
