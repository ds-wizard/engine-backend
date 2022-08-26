module Wizard.Model.Branch.BranchList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Branch.BranchState

data BranchList =
  BranchList
    { _branchListUuid :: U.UUID
    , _branchListName :: String
    , _branchListKmId :: String
    , _branchListState :: BranchState
    , _branchListPreviousPackageId :: Maybe String
    , _branchListForkOfPackageId :: Maybe String
    , _branchListOwnerUuid :: Maybe U.UUID
    , _branchListCreatedAt :: UTCTime
    , _branchListUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
