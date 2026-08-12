module Wizard.Model.Project.ProjectState where

import GHC.Generics

data KnowledgeModelProjectState
  = UpToDateKnowledgeModelProjectState
  | OutdatedKnowledgeModelProjectState
  deriving (Show, Eq, Generic, Read)

data DocumentTemplateProjectState
  = UpToDateDocumentTemplateProjectState
  | OutdatedDocumentTemplateProjectState
  deriving (Show, Eq, Generic, Read)
