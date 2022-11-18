module Shared.Api.Resource.Template.TemplateSuggestionDTO where

import GHC.Generics

import Shared.Api.Resource.Template.TemplateFormatDTO

data TemplateSuggestionDTO = TemplateSuggestionDTO
  { tId :: String
  , name :: String
  , version :: String
  , description :: String
  , formats :: [TemplateFormatDTO]
  }
  deriving (Show, Eq, Generic)
