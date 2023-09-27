module Wizard.Api.Resource.Tenant.Usage.TenantUsageJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Tenant.Usage.TenantUsageDTO

instance FromJSON TenantUsageDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantUsageDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON TenantUsageEntryDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantUsageEntryDTO where
  toJSON = genericToJSON jsonOptions
