module Wizard.Api.Resource.Project.ProjectCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Project.Project

data ProjectCreateDTO = ProjectCreateDTO
  { name :: String
  , knowledgeModelPackageId :: String
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , questionTagUuids :: [U.UUID]
  , documentTemplateId :: Maybe String
  , formatUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)
