module Service.Branch.BranchMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Branch.BranchChangeDTO
import Api.Resource.Branch.BranchDTO
import Api.Resource.Branch.BranchWithStateDTO
import Api.Resource.Organization.OrganizationDTO
import LensesConfig
import Model.Branch.Branch
import Model.Branch.BranchState

toDTO :: Branch -> OrganizationDTO -> BranchDTO
toDTO branch organization =
  BranchDTO
  { _branchDTOUuid = branch ^. uuid
  , _branchDTOName = branch ^. name
  , _branchDTOOrganizationId = organization ^. organizationId
  , _branchDTOKmId = branch ^. kmId
  , _branchDTOParentPackageId = branch ^. parentPackageId
  , _branchDTOLastAppliedParentPackageId = branch ^. lastAppliedParentPackageId
  , _branchDTOOwnerUuid = branch ^. ownerUuid
  , _branchDTOCreatedAt = branch ^. createdAt
  , _branchDTOUpdatedAt = branch ^. updatedAt
  }

toWithStateDTO :: Branch -> BranchState -> OrganizationDTO -> BranchWithStateDTO
toWithStateDTO branch state organization =
  BranchWithStateDTO
  { _branchWithStateDTOUuid = branch ^. uuid
  , _branchWithStateDTOName = branch ^. name
  , _branchWithStateDTOOrganizationId = organization ^. organizationId
  , _branchWithStateDTOKmId = branch ^. kmId
  , _branchWithStateDTOParentPackageId = branch ^. parentPackageId
  , _branchWithStateDTOLastAppliedParentPackageId = branch ^. lastAppliedParentPackageId
  , _branchWithStateDTOState = state
  , _branchWithStateDTOOwnerUuid = branch ^. ownerUuid
  , _branchWithStateDTOCreatedAt = branch ^. createdAt
  , _branchWithStateDTOUpdatedAt = branch ^. updatedAt
  }

fromChangeDTO :: BranchChangeDTO -> U.UUID -> Maybe String -> Maybe U.UUID -> UTCTime -> UTCTime -> Branch
fromChangeDTO dto bUuid bLastAppliedParentPackageId mOwnerUuid bCreatedAt bUpdatedAt =
  Branch
  { _branchUuid = bUuid
  , _branchName = dto ^. name
  , _branchKmId = dto ^. kmId
  , _branchParentPackageId = dto ^. parentPackageId
  , _branchLastAppliedParentPackageId = bLastAppliedParentPackageId
  , _branchLastMergeCheckpointPackageId = Nothing
  , _branchOwnerUuid = mOwnerUuid
  , _branchCreatedAt = bCreatedAt
  , _branchUpdatedAt = bUpdatedAt
  }
