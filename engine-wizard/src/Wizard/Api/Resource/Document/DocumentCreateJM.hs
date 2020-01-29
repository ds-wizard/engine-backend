module Wizard.Api.Resource.Document.DocumentCreateJM where

import Data.Aeson

import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON DocumentCreateDTO where
  parseJSON = simpleParseJSON "_documentCreateDTO"

instance ToJSON DocumentCreateDTO where
  toJSON = simpleToJSON "_documentCreateDTO"
