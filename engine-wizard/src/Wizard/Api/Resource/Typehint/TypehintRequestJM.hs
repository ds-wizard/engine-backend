module Wizard.Api.Resource.Typehint.TypehintRequestJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventJM ()
import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Typehint.TypehintRequestDTO

instance FromJSON TypehintRequestDTO where
  parseJSON = simpleParseJSON "_typehintRequestDTO"

instance ToJSON TypehintRequestDTO where
  toJSON = simpleToJSON "_typehintRequestDTO"
