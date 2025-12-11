module Wizard.Api.Resource.Project.Action.ProjectActionChangeDTO where

import GHC.Generics

data ProjectActionChangeDTO = ProjectActionChangeDTO
  { enabled :: Bool
  }
  deriving (Show, Eq, Generic)
