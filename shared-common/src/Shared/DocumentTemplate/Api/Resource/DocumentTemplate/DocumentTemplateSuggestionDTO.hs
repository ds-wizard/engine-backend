module Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple

data DocumentTemplateSuggestionDTO = DocumentTemplateSuggestionDTO
  { uuid :: U.UUID
  , name :: String
  , organizationId :: String
  , templateId :: String
  , version :: String
  , description :: String
  , formats :: [DocumentTemplateFormatSimple]
  }
  deriving (Show, Eq, Generic)
