module Wizard.Model.Branch.BranchData where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.Event.Event

data BranchData =
  BranchData
    { _branchDataBranchUuid :: U.UUID
    , _branchDataMetamodelVersion :: Int
    , _branchDataEvents :: [Event]
    , _branchDataAppUuid :: U.UUID
    , _branchDataCreatedAt :: UTCTime
    , _branchDataUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
