module Wizard.Api.Resource.Template.File.TemplateFileChangeDTO where

import GHC.Generics

data TemplateFileChangeDTO = TemplateFileChangeDTO
  { fileName :: String
  , content :: String
  }
  deriving (Show, Eq, Generic)
