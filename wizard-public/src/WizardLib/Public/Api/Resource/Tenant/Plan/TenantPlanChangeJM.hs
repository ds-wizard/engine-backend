module WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanChangeDTO

instance FromJSON TenantPlanChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TenantPlanChangeDTO where
  toJSON = genericToJSON jsonOptions
