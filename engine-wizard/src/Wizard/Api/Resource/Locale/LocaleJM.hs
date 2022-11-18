module Wizard.Api.Resource.Locale.LocaleJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Locale.LocaleDTO

instance FromJSON LocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleDTO where
  toJSON = genericToJSON jsonOptions
