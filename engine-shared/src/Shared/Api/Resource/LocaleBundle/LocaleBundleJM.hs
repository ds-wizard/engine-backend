module Shared.Api.Resource.LocaleBundle.LocaleBundleJM where

import Data.Aeson

import Shared.Api.Resource.LocaleBundle.LocaleBundleDTO
import Shared.Util.Aeson

instance FromJSON LocaleBundleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleBundleDTO where
  toJSON = genericToJSON jsonOptions
