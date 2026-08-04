module Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList where

import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelLocaleList = KnowledgeModelLocaleList
  { uuid :: U.UUID
  , name :: String
  , code :: String
  }
  deriving (Show, Eq, Generic)
