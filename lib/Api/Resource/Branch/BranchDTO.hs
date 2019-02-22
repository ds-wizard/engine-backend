module Api.Resource.Branch.BranchDTO where

import Data.Time
import qualified Data.UUID as U

import Model.Branch.BranchState

data BranchDTO = BranchDTO
  { _branchDTOUuid :: U.UUID
  , _branchDTOName :: String
  , _branchDTOOrganizationId :: String
  , _branchDTOKmId :: String
  , _branchDTOState :: BranchState
  , _branchDTOParentPackageId :: Maybe String
  , _branchDTOLastAppliedParentPackageId :: Maybe String
  , _branchDTOOwnerUuid :: Maybe U.UUID
  , _branchDTOCreatedAt :: UTCTime
  , _branchDTOUpdatedAt :: UTCTime
  } deriving (Show)
