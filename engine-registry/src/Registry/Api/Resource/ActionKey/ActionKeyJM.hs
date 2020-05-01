module Registry.Api.Resource.ActionKey.ActionKeyJM where

import Data.Aeson

import Registry.Api.Resource.ActionKey.ActionKeyDTO
import Registry.Model.ActionKey.ActionKey
import Shared.Util.JSON

instance FromJSON ActionKeyType

instance ToJSON ActionKeyType

instance FromJSON ActionKeyDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON ActionKeyDTO where
  toJSON = genericToJSON simpleOptions
