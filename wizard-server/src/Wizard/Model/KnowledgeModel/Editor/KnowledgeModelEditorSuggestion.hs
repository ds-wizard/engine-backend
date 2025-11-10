module Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion where

import qualified Data.UUID as U
import GHC.Generics

data KnowledgeModelEditorSuggestion = KnowledgeModelEditorSuggestion
  { uuid :: U.UUID
  , name :: String
  }
  deriving (Generic, Eq, Show)
