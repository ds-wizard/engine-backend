module Wizard.Api.Resource.Project.ProjectDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple
import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectState

data ProjectDTO = ProjectDTO
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , state :: ProjectState
  , knowledgeModelPackage :: KnowledgeModelPackageSimple
  , permissions :: [ProjectPermDTO]
  , isTemplate :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq ProjectDTO where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.description == b.description
      && a.visibility == b.visibility
      && a.sharing == b.sharing
      && a.state == b.state
      && a.knowledgeModelPackage == b.knowledgeModelPackage
      && a.permissions == b.permissions
      && a.isTemplate == b.isTemplate
