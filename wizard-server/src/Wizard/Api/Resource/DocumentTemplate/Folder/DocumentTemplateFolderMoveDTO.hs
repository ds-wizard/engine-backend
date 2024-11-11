module Wizard.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveDTO where

import GHC.Generics

data DocumentTemplateFolderMoveDTO = DocumentTemplateFolderMoveDTO
  { current :: String
  , new :: String
  }
  deriving (Show, Eq, Generic)
