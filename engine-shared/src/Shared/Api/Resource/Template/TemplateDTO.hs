module Shared.Api.Resource.Template.TemplateDTO where

import GHC.Generics

import Shared.Api.Resource.Template.TemplateFormatDTO

data TemplateDTO =
  TemplateDTO
    { _templateDTOTId :: String
    , _templateDTOName :: String
    , _templateDTOVersion :: String
    , _templateDTODescription :: String
    , _templateDTOFormats :: [TemplateFormatDTO]
    }
  deriving (Show, Eq, Generic)
