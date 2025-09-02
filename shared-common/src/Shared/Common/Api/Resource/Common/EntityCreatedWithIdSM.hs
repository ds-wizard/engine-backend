module Shared.Common.Api.Resource.Common.EntityCreatedWithIdSM where

import Data.Swagger

import Shared.Common.Api.Resource.Common.EntityCreatedWithIdDTO
import Shared.Common.Api.Resource.Common.EntityCreatedWithIdJM ()
import Shared.Common.Util.Swagger

instance ToSchema EntityCreatedWithIdDTO where
  declareNamedSchema = toSwagger (EntityCreatedWithIdDTO {aId = "org:entity:1.0.0"})
