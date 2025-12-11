module Wizard.Api.Resource.Project.ProjectCreateFromTemplateDTO where

import qualified Data.UUID as U
import GHC.Generics

data ProjectCreateFromTemplateDTO = ProjectCreateFromTemplateDTO
  { name :: String
  , projectUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
