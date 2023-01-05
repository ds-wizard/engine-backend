module Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO where

import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateFormatDTO = DocumentTemplateFormatDTO
  { uuid :: U.UUID
  , name :: String
  , icon :: String
  , isPdf :: Bool
  }
  deriving (Show, Eq, Generic)
