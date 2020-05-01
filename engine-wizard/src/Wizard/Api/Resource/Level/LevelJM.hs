module Wizard.Api.Resource.Level.LevelJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Level.LevelDTO

instance FromJSON LevelDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON LevelDTO where
  toJSON = genericToJSON simpleOptions
