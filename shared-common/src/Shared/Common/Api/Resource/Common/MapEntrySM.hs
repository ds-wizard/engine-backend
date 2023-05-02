module Shared.Common.Api.Resource.Common.MapEntrySM where

import Data.Swagger
import Shared.Common.Api.Resource.Common.MapEntryJM ()
import Shared.Common.Model.Common.MapEntry
import Shared.Common.Util.Swagger

instance (ToSchema key, ToSchema value) => ToSchema (MapEntry key value) where
  declareNamedSchema = toSwagger (MapEntry "key" "value")
