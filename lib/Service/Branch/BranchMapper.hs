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
  { _bdtoUuid = branch ^. bUuid
  , _bdtoName = branch ^. bName
  , _bdtoOrganizationId = organization ^. organizationId
  , _bdtoKmId = branch ^. bKmId
  , _bdtoParentPackageId = branch ^. bParentPackageId
  , _bdtoLastAppliedParentPackageId = branch ^. bLastAppliedParentPackageId
  }

toWithStateDTO :: Branch -> BranchState -> OrganizationDTO -> BranchWithStateDTO
toWithStateDTO branch state organization =
  BranchWithStateDTO
  { _bwsdtoUuid = branch ^. bUuid
  , _bwsdtoName = branch ^. bName
  , _bwsdtoOrganizationId = organization ^. organizationId
  , _bwsdtoKmId = branch ^. bKmId
  , _bwsdtoParentPackageId = branch ^. bParentPackageId
  , _bwsdtoLastAppliedParentPackageId = branch ^. bLastAppliedParentPackageId
  , _bwsdtoState = state
  }

fromDTO :: BranchDTO -> Branch
fromDTO dto =
  Branch
  { _bUuid = dto ^. bdtoUuid
  , _bName = dto ^. bdtoName
  , _bKmId = dto ^. bdtoKmId
  , _bParentPackageId = dto ^. bdtoParentPackageId
  , _bLastAppliedParentPackageId = dto ^. bdtoLastAppliedParentPackageId
  , _bLastMergeCheckpointPackageId = Nothing
  }
