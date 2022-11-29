module Wizard.Model.Branch.BranchData where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event

data BranchData = BranchData
  { branchUuid :: U.UUID
  , metamodelVersion :: Int
  , events :: [Event]
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
