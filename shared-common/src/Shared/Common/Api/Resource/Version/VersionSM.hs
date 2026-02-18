module Shared.Common.Api.Resource.Version.VersionSM where

import Data.Swagger

import Shared.Common.Api.Resource.Version.VersionDTO
import Shared.Common.Api.Resource.Version.VersionJM ()
import Shared.Common.Service.Version.VersionMapper
import Shared.Common.Util.Swagger
import Shared.Common.Util.Uuid

instance ToSchema VersionDTO where
  declareNamedSchema =
    toSwagger
      ( toVersionDTO (u' "ac3a6934-2069-4792-943c-e1170edee8c2", "1.0.0")
      )
