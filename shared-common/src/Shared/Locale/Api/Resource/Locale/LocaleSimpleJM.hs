module Shared.Locale.Api.Resource.Locale.LocaleSimpleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.Locale.Model.Locale.LocaleSimple

instance FromJSON LocaleSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleSimple where
  toJSON = genericToJSON jsonOptions
