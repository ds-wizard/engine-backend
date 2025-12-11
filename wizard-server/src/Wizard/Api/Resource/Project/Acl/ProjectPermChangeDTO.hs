module Wizard.Api.Resource.Project.Acl.ProjectPermChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Project.Acl.ProjectPerm

data ProjectPermChangeDTO = ProjectPermChangeDTO
  { memberType :: ProjectPermType
  , memberUuid :: U.UUID
  , perms :: [String]
  }
  deriving (Show, Eq, Generic)
