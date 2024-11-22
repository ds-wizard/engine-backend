module Shared.Common.Api.Resource.Tenant.Usage.UsageEntryDTO where

import GHC.Generics
import GHC.Int

data UsageEntryDTO = UsageEntryDTO
  { current :: Int64
  , max :: Int64
  }
  deriving (Show, Eq, Generic)
