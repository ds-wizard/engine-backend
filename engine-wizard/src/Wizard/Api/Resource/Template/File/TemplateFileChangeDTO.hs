module Wizard.Api.Resource.Template.File.TemplateFileChangeDTO where

import GHC.Generics

data TemplateFileChangeDTO =
  TemplateFileChangeDTO
    { _templateFileChangeDTOFileName :: String
    , _templateFileChangeDTOContent :: String
    }
  deriving (Show, Eq, Generic)
