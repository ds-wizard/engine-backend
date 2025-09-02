module WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO where

import GHC.Generics

import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple

data DocumentTemplateSuggestionDTO = DocumentTemplateSuggestionDTO
  { tId :: String
  , name :: String
  , version :: String
  , description :: String
  , formats :: [DocumentTemplateFormatSimple]
  }
  deriving (Show, Eq, Generic)
