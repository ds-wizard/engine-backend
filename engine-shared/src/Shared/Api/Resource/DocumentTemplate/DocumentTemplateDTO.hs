module Shared.Api.Resource.DocumentTemplate.DocumentTemplateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO
import Shared.Model.DocumentTemplate.DocumentTemplate

data DocumentTemplateDTO = DocumentTemplateDTO
  { tId :: String
  , name :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , description :: String
  , formats :: [DocumentTemplateFormatDTO]
  }
  deriving (Show, Eq, Generic)

data DocumentTemplateFileDTO = DocumentTemplateFileDTO
  { uuid :: U.UUID
  , fileName :: String
  , content :: String
  }
  deriving (Show, Eq, Generic)

data DocumentTemplateAssetDTO = DocumentTemplateAssetDTO
  { uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  }
  deriving (Show, Eq, Generic)
