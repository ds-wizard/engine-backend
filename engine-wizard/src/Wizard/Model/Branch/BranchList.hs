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
  deriving (Show, Generic)

instance Eq BranchList where
  a == b =
    _branchListUuid a == _branchListUuid b &&
    _branchListName a == _branchListName b &&
    _branchListKmId a == _branchListKmId b &&
    _branchListState a == _branchListState b &&
    _branchListPreviousPackageId a == _branchListPreviousPackageId b &&
    _branchListForkOfPackageId a == _branchListForkOfPackageId b && _branchListOwnerUuid a == _branchListOwnerUuid b
