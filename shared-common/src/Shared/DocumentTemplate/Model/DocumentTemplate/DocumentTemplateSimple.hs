module Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateSimple where

import qualified Data.UUID as U
import GHC.Generics

data DocumentTemplateSimple = DocumentTemplateSimple
  { uuid :: U.UUID
  , name :: String
  }
  deriving (Show, Eq, Generic)
