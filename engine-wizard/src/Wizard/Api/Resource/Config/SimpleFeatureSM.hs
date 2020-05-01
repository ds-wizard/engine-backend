module Wizard.Api.Resource.Config.SimpleFeatureSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Config.SimpleFeatureJM ()
import Wizard.Model.Config.SimpleFeature

instance ToSchema SimpleFeature where
  declareNamedSchema = simpleToSchema' "_simpleFeature" (SimpleFeature True)
