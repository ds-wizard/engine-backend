module Shared.Api.Resource.Template.TemplateSuggestionDTO where

import GHC.Generics

import Shared.Api.Resource.Template.TemplateFormatDTO

data TemplateSuggestionDTO =
  TemplateSuggestionDTO
    { _templateSuggestionDTOTId :: String
    , _templateSuggestionDTOName :: String
    , _templateSuggestionDTOVersion :: String
    , _templateSuggestionDTODescription :: String
    , _templateSuggestionDTOFormats :: [TemplateFormatDTO]
    }
  deriving (Show, Eq, Generic)
