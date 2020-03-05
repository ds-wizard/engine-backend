module Wizard.Api.Resource.Template.TemplateJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Template.TemplateDTO

instance FromJSON TemplateDTO where
  parseJSON = simpleParseJSON "_templateDTO"

instance ToJSON TemplateDTO where
  toJSON = simpleToJSON "_templateDTO"

instance FromJSON TemplateAllowedKMDTO where
  parseJSON = simpleParseJSON "_templateAllowedKMDTO"

instance ToJSON TemplateAllowedKMDTO where
  toJSON = simpleToJSON "_templateAllowedKMDTO"
