module Shared.ActionKey.Api.Resource.ActionKey.ActionKeyJM where

import Data.Aeson

import Shared.ActionKey.Api.Resource.ActionKey.ActionKeyDTO
import Shared.Common.Util.Aeson

instance FromJSON aType => FromJSON (ActionKeyDTO aType) where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON aType => ToJSON (ActionKeyDTO aType) where
  toJSON = genericToJSON jsonOptions
