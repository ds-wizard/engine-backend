module Shared.Api.Resource.Template.TemplateSuggestionJM where

import Data.Aeson

import Shared.Api.Resource.Template.TemplateFormatJM ()
import Shared.Api.Resource.Template.TemplateSuggestionDTO
import Shared.Util.Aeson

instance FromJSON TemplateSuggestionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TemplateSuggestionDTO where
  toJSON = genericToJSON jsonOptions
