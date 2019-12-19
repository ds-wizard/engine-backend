module Registry.Api.Resource.ActionKey.ActionKeyJM where

import Data.Aeson

import Registry.Api.Resource.ActionKey.ActionKeyDTO
import Registry.Model.ActionKey.ActionKey
import Registry.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON ActionKeyType

instance ToJSON ActionKeyType

instance FromJSON ActionKeyDTO where
  parseJSON = simpleParseJSON "_actionKeyDTO"

instance ToJSON ActionKeyDTO where
  toJSON = simpleToJSON "_actionKeyDTO"
