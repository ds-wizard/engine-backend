module Wizard.Api.Resource.Tenant.Plan.TenantPlanJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Tenant.Plan.TenantPlan

instance FromJSON TenantPlan where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantPlan where
  toJSON = genericToJSON jsonOptions
