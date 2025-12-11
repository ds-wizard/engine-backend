module Wizard.Api.Resource.Project.ProjectShareChangeDTO where

import GHC.Generics

import Wizard.Api.Resource.Project.Acl.ProjectPermChangeDTO
import Wizard.Model.Project.Project

data ProjectShareChangeDTO = ProjectShareChangeDTO
  { visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , permissions :: [ProjectPermChangeDTO]
  }
  deriving (Show, Eq, Generic)
