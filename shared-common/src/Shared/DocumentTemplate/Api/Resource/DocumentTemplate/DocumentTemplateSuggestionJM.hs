module Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleJM ()
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO

instance FromJSON DocumentTemplateSuggestionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateSuggestionDTO where
  toJSON = genericToJSON jsonOptions
