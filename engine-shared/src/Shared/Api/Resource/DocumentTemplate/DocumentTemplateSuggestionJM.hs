module Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import Shared.Util.Aeson

instance FromJSON DocumentTemplateSuggestionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateSuggestionDTO where
  toJSON = genericToJSON jsonOptions
