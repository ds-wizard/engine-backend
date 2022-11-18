module Shared.Api.Resource.Common.MapEntryJM where

import Data.Aeson

import Shared.Model.Common.MapEntry
import Shared.Util.Aeson

instance (ToJSON key, ToJSON value) => ToJSON (MapEntry key value) where
  toJSON = genericToJSON jsonOptions

instance (FromJSON key, FromJSON value) => FromJSON (MapEntry key value) where
  parseJSON = genericParseJSON jsonOptions
