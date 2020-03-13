module Wizard.Api.Resource.Config.SimpleFeatureJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Config.SimpleFeatureDTO

instance FromJSON SimpleFeatureDTO where
  parseJSON = simpleParseJSON "_simpleFeatureDTO"

instance ToJSON SimpleFeatureDTO where
  toJSON = simpleToJSON "_simpleFeatureDTO"
