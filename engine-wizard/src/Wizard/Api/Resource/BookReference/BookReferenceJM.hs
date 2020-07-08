module Wizard.Api.Resource.BookReference.BookReferenceJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.BookReference.BookReference

instance FromJSON BookReference where
  parseJSON = simpleParseJSON "_bookReference"

instance ToJSON BookReference where
  toJSON = simpleToJSON "_bookReference"
