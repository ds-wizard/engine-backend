module Wizard.Model.Branch.BranchDataLength where

import qualified Data.UUID as U
import GHC.Generics

data BranchDataLength =
  BranchDataLength
    { _branchDataLengthBranchUuid :: U.UUID
    , _branchDataLengthEventSize :: Int
    }
  deriving (Show, Eq, Generic)
