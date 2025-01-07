module Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteDTO where

import GHC.Generics

data DocumentTemplateFolderDeleteDTO = DocumentTemplateFolderDeleteDTO
  { path :: String
  }
  deriving (Show, Eq, Generic)
