module Shared.Api.Resource.Common.MapEntrySM where

import Data.Swagger
import Shared.Api.Resource.Common.MapEntryJM ()
import Shared.Model.Common.MapEntry
import Shared.Util.Swagger

instance (ToSchema key, ToSchema value) => ToSchema (MapEntry key value) where
  declareNamedSchema = toSwagger (MapEntry "key" "value")
