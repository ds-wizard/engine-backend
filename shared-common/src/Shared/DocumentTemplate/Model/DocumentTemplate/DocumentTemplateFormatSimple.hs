module Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple where

import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateFormatSimple = DocumentTemplateFormatSimple
  { uuid :: U.UUID
  , name :: String
  , icon :: String
  }
  deriving (Show, Eq, Generic)
