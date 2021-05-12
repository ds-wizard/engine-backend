module Shared.Api.Resource.Template.TemplateDTO where

import qualified Data.UUID as U
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

data TemplateFileDTO =
  TemplateFileDTO
    { _templateFileDTOUuid :: U.UUID
    , _templateFileDTOFileName :: String
    , _templateFileDTOContent :: String
    }
  deriving (Show, Eq, Generic)

data TemplateAssetDTO =
  TemplateAssetDTO
    { _templateAssetDTOUuid :: U.UUID
    , _templateAssetDTOFileName :: String
    , _templateAssetDTOContentType :: String
    }
  deriving (Show, Eq, Generic)
