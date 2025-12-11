module Wizard.Api.Resource.Project.Version.ProjectVersionRevertDTO where

import qualified Data.UUID as U
import GHC.Generics

data ProjectVersionRevertDTO = ProjectVersionRevertDTO
  { eventUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
