module Wizard.Api.Resource.BookReference.BookReferenceJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.BookReference.BookReferenceDTO

instance FromJSON BookReferenceDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON BookReferenceDTO where
  toJSON = genericToJSON simpleOptions
