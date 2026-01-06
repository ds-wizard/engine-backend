module Wizard.Api.Resource.Project.Action.ProjectActionDTO where

import Data.Time
import GHC.Generics

data ProjectActionDTO = ProjectActionDTO
  { paId :: String
  , name :: String
  , description :: String
  , url :: String
  , enabled :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
