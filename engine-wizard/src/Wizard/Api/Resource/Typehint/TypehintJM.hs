module Wizard.Api.Resource.Typehint.TypehintJM where

import Data.Aeson

import Wizard.Api.Resource.Typehint.TypehintDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON TypehintDTO where
  parseJSON = simpleParseJSON "_typehintDTO"

instance ToJSON TypehintDTO where
  toJSON = simpleToJSON "_typehintDTO"
