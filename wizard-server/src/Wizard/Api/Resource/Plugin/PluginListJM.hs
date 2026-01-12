module Wizard.Api.Resource.Plugin.PluginListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.Plugin.PluginList

instance FromJSON PluginList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PluginList where
  toJSON = genericToJSON jsonOptions
