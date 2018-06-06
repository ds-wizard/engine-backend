module Service.Branch.BranchMapper where

import Control.Lens ((^.))

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
  }

fromDTO :: BranchDTO -> Branch
fromDTO dto =
  Branch
  { _branchUuid = dto ^. uuid
  , _branchName = dto ^. name
  , _branchKmId = dto ^. kmId
  , _branchParentPackageId = dto ^. parentPackageId
  , _branchLastAppliedParentPackageId = dto ^. lastAppliedParentPackageId
  , _branchLastMergeCheckpointPackageId = Nothing
  }
