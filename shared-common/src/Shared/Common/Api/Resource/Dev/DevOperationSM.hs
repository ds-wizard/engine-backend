module Shared.Common.Api.Resource.Dev.DevOperationSM where

import Data.Swagger

import Shared.Common.Api.Resource.Dev.DevOperationDTO
import Shared.Common.Api.Resource.Dev.DevOperationJM ()
import Shared.Common.Api.Resource.Dev.DevSM ()
import Shared.Common.Database.Migration.Development.Dev.Data.Devs
import Shared.Common.Util.Swagger

instance ToSchema DevOperationDTO where
  declareNamedSchema = toSwagger operationDto
