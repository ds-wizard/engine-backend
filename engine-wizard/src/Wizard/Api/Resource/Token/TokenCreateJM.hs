module Wizard.Api.Resource.Token.TokenCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Token.TokenCreateDTO

instance FromJSON TokenCreateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TokenCreateDTO where
  toJSON = genericToJSON simpleOptions
