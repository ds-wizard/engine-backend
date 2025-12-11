module Wizard.Api.Resource.Project.Importer.ProjectImporterDTO where

import Data.Time
import GHC.Generics

data ProjectImporterDTO = ProjectImporterDTO
  { piId :: String
  , name :: String
  , description :: String
  , url :: String
  , enabled :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
