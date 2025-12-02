module Wizard.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorList where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Wizard.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorState ()
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList

instance FromRow KnowledgeModelEditorList where
  fromRow = do
    uuid <- field
    name <- field
    kmId <- field
    version <- field
    state <- field
    previousPackageId <- field
    forkOfPackageId <- field
    createdBy <- field
    createdAt <- field
    updatedAt <- field
    return $ KnowledgeModelEditorList {..}
