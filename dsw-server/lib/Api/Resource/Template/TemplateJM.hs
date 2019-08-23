module Api.Resource.Template.TemplateJM where

import Data.Aeson

import Api.Resource.Template.TemplateDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON TemplateDTO where
  parseJSON = simpleParseJSON "_templateDTO"

instance ToJSON TemplateDTO where
  toJSON = simpleToJSON "_templateDTO"
