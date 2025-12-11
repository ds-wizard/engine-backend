module Wizard.Model.Project.ProjectState where

import GHC.Generics

data ProjectState
  = DefaultProjectState
  | MigratingProjectState
  | OutdatedProjectState
  deriving (Show, Eq, Generic, Read)
