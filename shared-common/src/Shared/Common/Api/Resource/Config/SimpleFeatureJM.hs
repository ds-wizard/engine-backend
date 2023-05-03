module Shared.Common.Api.Resource.Config.SimpleFeatureJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.Common.Model.Config.SimpleFeature

instance FromJSON SimpleFeature where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SimpleFeature where
  toJSON = genericToJSON jsonOptions
