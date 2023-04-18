module WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO where

import GHC.Generics

import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO

data DocumentTemplateSuggestionDTO = DocumentTemplateSuggestionDTO
  { tId :: String
  , name :: String
  , version :: String
  , description :: String
  , formats :: [DocumentTemplateFormatDTO]
  }
  deriving (Show, Eq, Generic)
