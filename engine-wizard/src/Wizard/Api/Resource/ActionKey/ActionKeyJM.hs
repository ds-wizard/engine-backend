module Wizard.Api.Resource.ActionKey.ActionKeyJM where

import Data.Aeson

import Wizard.Api.Resource.ActionKey.ActionKeyDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON ActionKeyDTO where
  parseJSON = simpleParseJSON "_actionKeyDTO"

instance ToJSON ActionKeyDTO where
  toJSON = simpleToJSON "_actionKeyDTO"
