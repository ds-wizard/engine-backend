module Wizard.Api.Resource.ActionKey.ActionKeyJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.ActionKey.ActionKeyDTO
import Wizard.Model.ActionKey.ActionKey

instance FromJSON ActionKeyType

instance ToJSON ActionKeyType

instance FromJSON ActionKeyDTO where
  parseJSON = simpleParseJSON "_actionKeyDTO"

instance ToJSON ActionKeyDTO where
  toJSON = simpleToJSON "_actionKeyDTO"
