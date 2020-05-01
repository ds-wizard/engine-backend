module Wizard.Api.Resource.Typehint.TypehintJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Typehint.TypehintDTO

instance FromJSON TypehintDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TypehintDTO where
  toJSON = genericToJSON simpleOptions
