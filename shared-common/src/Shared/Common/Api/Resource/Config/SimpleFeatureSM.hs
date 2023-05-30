module Shared.Common.Api.Resource.Config.SimpleFeatureSM where

import Data.Swagger

import Shared.Common.Api.Resource.Config.SimpleFeatureJM ()
import Shared.Common.Model.Config.SimpleFeature
import Shared.Common.Util.Swagger

instance ToSchema SimpleFeature where
  declareNamedSchema = toSwagger (SimpleFeature True)
