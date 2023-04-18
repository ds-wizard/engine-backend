module Wizard.Api.Resource.Config.SimpleFeatureSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Config.SimpleFeatureJM ()
import Wizard.Model.Config.SimpleFeature

instance ToSchema SimpleFeature where
  declareNamedSchema = toSwagger (SimpleFeature True)
