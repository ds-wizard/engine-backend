module Api.Resource.Branch.BranchDTO where

import Data.Time
import qualified Data.UUID as U

data BranchDTO = BranchDTO
  { _branchDTOUuid :: U.UUID
  , _branchDTOName :: String
  , _branchDTOOrganizationId :: String
  , _branchDTOKmId :: String
  , _branchDTOParentPackageId :: Maybe String
  , _branchDTOLastAppliedParentPackageId :: Maybe String
  , _branchDTOOwnerUuid :: Maybe U.UUID
  , _branchDTOCreatedAt :: UTCTime
  , _branchDTOUpdatedAt :: UTCTime
  } deriving (Show)
