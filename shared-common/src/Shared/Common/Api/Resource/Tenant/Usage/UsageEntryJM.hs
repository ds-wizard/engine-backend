module Shared.Common.Api.Resource.Tenant.Usage.UsageEntryJM where

import Data.Aeson

import Shared.Common.Api.Resource.Tenant.Usage.UsageEntryDTO
import Shared.Common.Util.Aeson

instance FromJSON UsageEntryDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UsageEntryDTO where
  toJSON = genericToJSON jsonOptions
