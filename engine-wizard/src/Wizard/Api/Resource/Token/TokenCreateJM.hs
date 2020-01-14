module Wizard.Api.Resource.Token.TokenCreateJM where

import Data.Aeson

import Wizard.Api.Resource.Token.TokenCreateDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON TokenCreateDTO where
  parseJSON = simpleParseJSON "_tokenCreateDTO"

instance ToJSON TokenCreateDTO where
  toJSON = simpleToJSON "_tokenCreateDTO"
