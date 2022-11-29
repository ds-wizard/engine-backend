module Wizard.Api.Resource.Prefab.PrefabJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Model.Prefab.Prefab

instance FromJSON Prefab where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Prefab where
  toJSON = genericToJSON jsonOptions
