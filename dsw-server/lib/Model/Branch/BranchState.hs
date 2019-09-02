module Model.Branch.BranchState where

import GHC.Generics

data BranchState
  = BSDefault
  | BSEdited
  | BSOutdated
  | BSMigrating
  | BSMigrated
  deriving (Show, Eq, Generic)
