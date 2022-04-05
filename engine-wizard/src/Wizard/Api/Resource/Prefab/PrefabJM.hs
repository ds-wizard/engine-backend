module Wizard.Api.Resource.Prefab.PrefabJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.Prefab.Prefab

instance FromJSON Prefab where
  parseJSON = simpleParseJSON "_prefab"

instance ToJSON Prefab where
  toJSON = simpleToJSON "_prefab"
