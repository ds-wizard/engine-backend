module Wizard.Messaging.Resource.Document.DocumentCreateMJM where

import Data.Aeson

import Wizard.Api.Resource.Document.DocumentContextJM ()
import Wizard.Messaging.Resource.Document.DocumentCreateMDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON DocumentCreateMDTO where
  parseJSON = simpleParseJSON "_documentCreateMDTO"

instance ToJSON DocumentCreateMDTO where
  toJSON = simpleToJSON "_documentCreateMDTO"
