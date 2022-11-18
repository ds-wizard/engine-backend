module Wizard.Api.Resource.Config.SimpleFeatureJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Model.Config.SimpleFeature

instance FromJSON SimpleFeature where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SimpleFeature where
  toJSON = genericToJSON jsonOptions
