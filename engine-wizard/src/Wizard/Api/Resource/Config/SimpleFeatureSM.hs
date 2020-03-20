module Wizard.Api.Resource.Config.SimpleFeatureSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Config.SimpleFeatureDTO
import Wizard.Api.Resource.Config.SimpleFeatureJM ()

instance ToSchema SimpleFeatureDTO where
  declareNamedSchema = simpleToSchema "_simpleFeatureDTO" (SimpleFeatureDTO True)
