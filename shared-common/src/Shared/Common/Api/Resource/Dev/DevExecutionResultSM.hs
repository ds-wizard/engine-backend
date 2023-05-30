module Shared.Common.Api.Resource.Dev.DevExecutionResultSM where

import Data.Swagger

import Shared.Common.Api.Resource.Dev.DevExecutionResultDTO
import Shared.Common.Api.Resource.Dev.DevExecutionResultJM ()
import Shared.Common.Database.Migration.Development.Dev.Data.Devs
import Shared.Common.Util.Swagger

instance ToSchema AdminExecutionResultDTO where
  declareNamedSchema = toSwagger execution1Result
