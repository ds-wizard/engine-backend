module Wizard.Api.Resource.Document.DocumentCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Document.DocumentCreateDTO

instance FromJSON DocumentCreateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON DocumentCreateDTO where
  toJSON = genericToJSON simpleOptions
