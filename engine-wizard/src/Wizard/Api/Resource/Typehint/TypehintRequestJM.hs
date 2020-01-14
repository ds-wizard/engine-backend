module Wizard.Api.Resource.Typehint.TypehintRequestJM where

import Data.Aeson

import Wizard.Api.Resource.Event.EventJM ()
import Wizard.Api.Resource.Typehint.TypehintRequestDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON TypehintRequestDTO where
  parseJSON = simpleParseJSON "_typehintRequestDTO"

instance ToJSON TypehintRequestDTO where
  toJSON = simpleToJSON "_typehintRequestDTO"
