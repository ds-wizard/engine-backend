module Wizard.Model.Project.Detail.ProjectDetail where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Wizard.Model.Project.Project

data ProjectDetail = ProjectDetail
  { uuid :: U.UUID
  , name :: String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , knowledgeModelPackage :: KnowledgeModelPackageSuggestion
  , selectedQuestionTagUuids :: [U.UUID]
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [ProjectPermDTO]
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
