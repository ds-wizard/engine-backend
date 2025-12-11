module Wizard.Model.Project.ProjectSimpleWithPerm where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Model.Project.Project

data ProjectSimpleWithPerm = ProjectSimpleWithPerm
  { uuid :: U.UUID
  , visibility :: ProjectVisibility
  , sharing :: ProjectSharing
  , tenantUuid :: U.UUID
  , permissions :: [ProjectPerm]
  }
  deriving (Generic, Eq, Show)
