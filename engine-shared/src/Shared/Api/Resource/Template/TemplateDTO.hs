module Shared.Api.Resource.Template.TemplateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Template.TemplateFormatDTO

data TemplateDTO = TemplateDTO
  { tId :: String
  , name :: String
  , version :: String
  , description :: String
  , formats :: [TemplateFormatDTO]
  }
  deriving (Show, Eq, Generic)

data TemplateFileDTO = TemplateFileDTO
  { uuid :: U.UUID
  , fileName :: String
  , content :: String
  }
  deriving (Show, Eq, Generic)

data TemplateAssetDTO = TemplateAssetDTO
  { uuid :: U.UUID
  , fileName :: String
  , contentType :: String
  }
  deriving (Show, Eq, Generic)
