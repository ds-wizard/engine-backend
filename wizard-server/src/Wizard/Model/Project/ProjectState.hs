module Wizard.Model.Project.ProjectState where

import GHC.Generics

data ProjectState
  = DefaultProjectState
  | OutdatedProjectState
  deriving (Show, Eq, Generic, Read)
