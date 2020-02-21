module Wizard.Api.Resource.Typehint.TypehintJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Typehint.TypehintDTO

instance FromJSON TypehintDTO where
  parseJSON = simpleParseJSON "_typehintDTO"

instance ToJSON TypehintDTO where
  toJSON = simpleToJSON "_typehintDTO"
