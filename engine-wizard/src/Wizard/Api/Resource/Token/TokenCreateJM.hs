module Wizard.Api.Resource.Token.TokenCreateJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Token.TokenCreateDTO

instance FromJSON TokenCreateDTO where
  parseJSON = simpleParseJSON "_tokenCreateDTO"

instance ToJSON TokenCreateDTO where
  toJSON = simpleToJSON "_tokenCreateDTO"
