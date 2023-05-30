module Shared.Common.Api.Resource.Dev.DevExecutionSM where

import Data.Swagger

import Shared.Common.Api.Resource.Dev.DevExecutionDTO
import Shared.Common.Api.Resource.Dev.DevExecutionJM ()
import Shared.Common.Database.Migration.Development.Dev.Data.Devs
import Shared.Common.Util.Swagger

instance ToSchema DevExecutionDTO where
  declareNamedSchema = toSwagger execution1
