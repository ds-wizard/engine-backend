module Api.Resource.Branch.BranchWithStateDTO where

import Data.Time
import qualified Data.UUID as U

import Model.Branch.BranchState

data BranchWithStateDTO = BranchWithStateDTO
  { _branchWithStateDTOUuid :: U.UUID
  , _branchWithStateDTOName :: String
  , _branchWithStateDTOOrganizationId :: String
  , _branchWithStateDTOKmId :: String
  , _branchWithStateDTOParentPackageId :: Maybe String
  , _branchWithStateDTOState :: BranchState
  , _branchWithStateDTOLastAppliedParentPackageId :: Maybe String
  , _branchWithStateDTOOwnerUuid :: Maybe U.UUID
  , _branchWithStateDTOCreatedAt :: UTCTime
  , _branchWithStateDTOUpdatedAt :: UTCTime
  }
