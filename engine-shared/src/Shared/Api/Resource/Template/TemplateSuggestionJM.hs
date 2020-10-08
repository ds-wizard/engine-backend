module Shared.Api.Resource.Template.TemplateSuggestionJM where

import Data.Aeson

import Shared.Api.Resource.Template.TemplateFormatJM ()
import Shared.Api.Resource.Template.TemplateSuggestionDTO
import Shared.Util.JSON

instance FromJSON TemplateSuggestionDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON TemplateSuggestionDTO where
  toJSON = genericToJSON simpleOptions
