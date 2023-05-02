module Wizard.Api.Resource.Locale.LocaleChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Locale.LocaleChangeDTO

instance FromJSON LocaleChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleChangeDTO where
  toJSON = genericToJSON jsonOptions
