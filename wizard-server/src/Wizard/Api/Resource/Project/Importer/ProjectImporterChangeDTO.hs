module Wizard.Api.Resource.Project.Importer.ProjectImporterChangeDTO where

import GHC.Generics

data ProjectImporterChangeDTO = ProjectImporterChangeDTO
  { enabled :: Bool
  }
  deriving (Show, Eq, Generic)
