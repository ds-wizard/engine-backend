module Wizard.Api.Resource.Token.TokenJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Token.TokenDTO

instance FromJSON TokenDTO where
  parseJSON = simpleParseJSON "_tokenDTO"

instance ToJSON TokenDTO where
  toJSON = simpleToJSON "_tokenDTO"
