module Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple

data DocumentTemplateDTO = DocumentTemplateDTO
  { tId :: String
  , name :: String
  , version :: String
  , phase :: DocumentTemplatePhase
  , description :: String
  , formats :: [DocumentTemplateFormatSimple]
  }
  deriving (Show, Eq, Generic)

data DocumentTemplateFormatDTO = DocumentTemplateFormatDTO
  { uuid :: U.UUID
  , name :: String
  , icon :: String
  , steps :: [DocumentTemplateFormatStepDTO]
  }
  deriving (Show, Eq, Generic)

data DocumentTemplateFormatStepDTO = DocumentTemplateFormatStepDTO
  { name :: String
  , options :: M.Map String String
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
