module Wizard.Api.Resource.Locale.LocaleJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Locale.LocaleDTO

instance FromJSON LocaleDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON LocaleDTO where
  toJSON = genericToJSON simpleOptions
