module Shared.Model.DocumentTemplate.DocumentTemplateFileList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateFileList = DocumentTemplateFileList
  { uuid :: U.UUID
  , fileName :: String
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
