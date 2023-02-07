module Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeDTO where

import GHC.Generics

data DocumentTemplateFileChangeDTO = DocumentTemplateFileChangeDTO
  { fileName :: String
  , content :: String
  }
  deriving (Show, Eq, Generic)
