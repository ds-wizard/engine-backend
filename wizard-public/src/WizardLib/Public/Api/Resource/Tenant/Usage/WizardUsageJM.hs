module WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageJM where

import Data.Aeson

import Shared.Common.Api.Resource.Tenant.Usage.UsageEntryJM ()
import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageDTO

instance FromJSON WizardUsageDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON WizardUsageDTO where
  toJSON = genericToJSON jsonOptions
