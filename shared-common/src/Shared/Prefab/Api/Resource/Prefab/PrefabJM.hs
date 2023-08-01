module Shared.Prefab.Api.Resource.Prefab.PrefabJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.Prefab.Model.Prefab.Prefab

instance FromJSON Prefab where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Prefab where
  toJSON = genericToJSON jsonOptions
