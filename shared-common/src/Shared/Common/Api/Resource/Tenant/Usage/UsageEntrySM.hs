module Shared.Common.Api.Resource.Tenant.Usage.UsageEntrySM where

import Data.Swagger

import Shared.Common.Api.Resource.Tenant.Usage.UsageEntryDTO
import Shared.Common.Api.Resource.Tenant.Usage.UsageEntryJM ()
import Shared.Common.Util.Swagger

instance ToSchema UsageEntryDTO where
  declareNamedSchema = toSwagger (UsageEntryDTO {current = 1, max = 10})
