module Wizard.Api.Resource.Document.DocumentCreateJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Document.DocumentCreateDTO

instance FromJSON DocumentCreateDTO where
  parseJSON = simpleParseJSON "_documentCreateDTO"

instance ToJSON DocumentCreateDTO where
  toJSON = simpleToJSON "_documentCreateDTO"
