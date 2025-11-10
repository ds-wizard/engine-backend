module Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorState where

import GHC.Generics

data KnowledgeModelEditorState
  = DefaultKnowledgeModelEditorState
  | EditedKnowledgeModelEditorState
  | OutdatedKnowledgeModelEditorState
  | MigratingKnowledgeModelEditorState
  | MigratedKnowledgeModelEditorState
  deriving (Show, Eq, Generic)
