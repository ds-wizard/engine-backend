module Shared.Common.Api.Resource.Dev.DevSM where

import Data.Swagger

import Shared.Common.Api.Resource.Dev.DevJM ()
import Shared.Common.Database.Migration.Development.Dev.Data.Devs
import Shared.Common.Model.Dev.Dev
import Shared.Common.Util.Swagger

instance ToSchema DevOperationParameter where
  declareNamedSchema = toSwagger operationParam1

instance ToSchema DevOperationParameterType
