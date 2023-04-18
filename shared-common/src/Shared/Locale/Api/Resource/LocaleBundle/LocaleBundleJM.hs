module Shared.Locale.Api.Resource.LocaleBundle.LocaleBundleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.Locale.Api.Resource.LocaleBundle.LocaleBundleDTO

instance FromJSON LocaleBundleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleBundleDTO where
  toJSON = genericToJSON jsonOptions
