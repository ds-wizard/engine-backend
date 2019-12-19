module Wizard.Api.Resource.Token.TokenJM where

import Data.Aeson

import Wizard.Api.Resource.Token.TokenDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON TokenDTO where
  parseJSON = simpleParseJSON "_tokenDTO"

instance ToJSON TokenDTO where
  toJSON = simpleToJSON "_tokenDTO"
