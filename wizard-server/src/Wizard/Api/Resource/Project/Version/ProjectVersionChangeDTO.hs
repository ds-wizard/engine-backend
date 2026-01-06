module Wizard.Api.Resource.Project.Version.ProjectVersionChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

data ProjectVersionChangeDTO = ProjectVersionChangeDTO
  { name :: String
  , description :: Maybe String
  , eventUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
