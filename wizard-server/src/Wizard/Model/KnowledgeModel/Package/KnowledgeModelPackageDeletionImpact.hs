module Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Wizard.Model.Project.ProjectSimple

data KnowledgeModelPackageDeletionImpact = KnowledgeModelPackageDeletionImpact
  { uuid :: U.UUID
  , name :: String
  , version :: String
  , packages :: [KnowledgeModelPackageReference]
  , editors :: [KnowledgeModelEditorSuggestion]
  , projects :: [ProjectSimple]
  }
  deriving (Show, Eq, Generic)

data KnowledgeModelPackageReference = KnowledgeModelPackageReference
  { uuid :: U.UUID
  , name :: String
  , version :: String
  }
  deriving (Show, Eq, Generic)
