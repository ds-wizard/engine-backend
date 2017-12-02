module Model.Branch.BranchState where

import GHC.Generics

data BranchState
  = BSDefault
  | BSEdited
  | BSOutdated
  | BSMigrating
  deriving (Show, Eq, Generic)
