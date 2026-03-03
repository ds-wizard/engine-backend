module Wizard.Api.Resource.Project.ProjectCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Project.Project

data ProjectCreateDTO = ProjectCreateDTO
  { name :: String
  , knowledgeModelPackageUuid :: U.UUID
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , questionTagUuids :: [U.UUID]
  , documentTemplateUuid :: Maybe U.UUID
  , formatUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
