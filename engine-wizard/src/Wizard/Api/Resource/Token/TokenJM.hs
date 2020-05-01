module Wizard.Api.Resource.Token.TokenJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Token.TokenDTO

instance FromJSON TokenDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TokenDTO where
  toJSON = genericToJSON simpleOptions
