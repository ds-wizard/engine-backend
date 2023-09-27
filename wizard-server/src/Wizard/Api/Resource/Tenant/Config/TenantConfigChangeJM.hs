module Wizard.Api.Resource.Tenant.Config.TenantConfigChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Api.Resource.Tenant.Config.TenantConfigJM ()

instance FromJSON TenantConfigChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantConfigChangeDTO where
  toJSON = genericToJSON jsonOptions
