module Wizard.Model.Project.Detail.ProjectDetail where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Model.Project.Project

data ProjectDetail = ProjectDetail
  { uuid :: U.UUID
  , name :: String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , knowledgeModelPackageId :: String
  , selectedQuestionTagUuids :: [U.UUID]
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [ProjectPermDTO]
  , projectActionsAvailable :: Int
  , projectImportersAvailable :: Int
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
