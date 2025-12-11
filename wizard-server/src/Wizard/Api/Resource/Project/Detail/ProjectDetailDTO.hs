module Wizard.Api.Resource.Project.Detail.ProjectDetailDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Model.Project.Project

data ProjectDetailDTO = ProjectDetailDTO
  { uuid :: U.UUID
  , name :: String
  , sharing :: ProjectSharing
  , visibility :: ProjectVisibility
  , knowledgeModelPackageId :: String
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , permissions :: [ProjectPermDTO]
  , fileCount :: Int
  }
  deriving (Show, Eq, Generic)
