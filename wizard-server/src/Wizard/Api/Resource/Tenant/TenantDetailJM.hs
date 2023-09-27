module Wizard.Api.Resource.Tenant.TenantDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Tenant.Plan.TenantPlanJM ()
import Wizard.Api.Resource.Tenant.TenantDetailDTO
import Wizard.Api.Resource.Tenant.Usage.TenantUsageJM ()
import Wizard.Api.Resource.User.UserJM ()

instance FromJSON TenantDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantDetailDTO where
  toJSON = genericToJSON jsonOptions
