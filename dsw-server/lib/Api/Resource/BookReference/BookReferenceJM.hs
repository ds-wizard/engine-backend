module Api.Resource.BookReference.BookReferenceJM where

import Data.Aeson

import Api.Resource.BookReference.BookReferenceDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON BookReferenceDTO where
  parseJSON = simpleParseJSON "_bookReferenceDTO"

instance ToJSON BookReferenceDTO where
  toJSON = simpleToJSON "_bookReferenceDTO"
