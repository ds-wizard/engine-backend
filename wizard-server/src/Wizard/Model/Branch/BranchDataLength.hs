module Wizard.Model.Branch.BranchDataLength where

import qualified Data.UUID as U
import GHC.Generics

data BranchDataLength = BranchDataLength
  { branchUuid :: U.UUID
  , eventSize :: Int
  }
  deriving (Show, Eq, Generic)
