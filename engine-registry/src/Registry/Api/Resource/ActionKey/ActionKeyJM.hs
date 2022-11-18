module Registry.Api.Resource.ActionKey.ActionKeyJM where

import Data.Aeson

import Registry.Api.Resource.ActionKey.ActionKeyDTO
import Registry.Model.ActionKey.ActionKey
import Shared.Util.Aeson

instance FromJSON ActionKeyType

instance ToJSON ActionKeyType

instance FromJSON ActionKeyDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ActionKeyDTO where
  toJSON = genericToJSON jsonOptions
