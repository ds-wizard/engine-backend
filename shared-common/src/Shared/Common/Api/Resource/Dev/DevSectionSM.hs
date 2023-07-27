module Shared.Common.Api.Resource.Dev.DevSectionSM where

import Data.Swagger

import Shared.Common.Api.Resource.Dev.DevOperationSM ()
import Shared.Common.Api.Resource.Dev.DevSectionDTO
import Shared.Common.Api.Resource.Dev.DevSectionJM ()
import Shared.Common.Database.Migration.Development.Dev.Data.Devs
import Shared.Common.Util.Swagger

instance ToSchema DevSectionDTO where
  declareNamedSchema = toSwagger sectionDto
