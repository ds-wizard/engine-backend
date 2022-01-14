module Shared.Api.Resource.Common.MapEntryJM where

import Data.Aeson

import Shared.Model.Common.MapEntry
import Shared.Util.JSON

instance (ToJSON key, ToJSON value) => ToJSON (MapEntry key value) where
  toJSON = simpleToJSON "_mapEntry"

instance (FromJSON key, FromJSON value) => FromJSON (MapEntry key value) where
  parseJSON = simpleParseJSON "_mapEntry"
