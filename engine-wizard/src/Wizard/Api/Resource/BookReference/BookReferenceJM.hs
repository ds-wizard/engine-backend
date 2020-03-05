module Wizard.Api.Resource.BookReference.BookReferenceJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.BookReference.BookReferenceDTO

instance FromJSON BookReferenceDTO where
  parseJSON = simpleParseJSON "_bookReferenceDTO"

instance ToJSON BookReferenceDTO where
  toJSON = simpleToJSON "_bookReferenceDTO"
