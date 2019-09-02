module Api.Resource.Typehint.TypehintRequestJM where

import Data.Aeson

import Api.Resource.Event.EventJM ()
import Api.Resource.Typehint.TypehintRequestDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON TypehintRequestDTO where
  parseJSON = simpleParseJSON "_typehintRequestDTO"

instance ToJSON TypehintRequestDTO where
  toJSON = simpleToJSON "_typehintRequestDTO"
