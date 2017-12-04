module Service.Branch.BranchMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resources.Branch.BranchDTO
import Api.Resources.Branch.BranchWithStateDTO
import Common.Types
import Model.Branch.Branch
import Model.Branch.BranchState
import Model.Organization.Organization

toDTO :: Branch -> Organization -> BranchDTO
toDTO branch organization =
  BranchDTO
  { _bdtoUuid = branch ^. bUuid
  , _bdtoName = branch ^. bName
  , _bdtoGroupId = organization ^. orgGroupId
  , _bdtoArtifactId = branch ^. bArtifactId
  , _bdtoParentPackageId = branch ^. bParentPackageId
  , _bdtoLastAppliedParentPackageId = branch ^. bLastAppliedParentPackageId
  }

toWithStateDTO :: Branch -> BranchState -> Organization -> BranchWithStateDTO
toWithStateDTO branch state organization =
  BranchWithStateDTO
  { _bwsdtoUuid = branch ^. bUuid
  , _bwsdtoName = branch ^. bName
  , _bwsdtoGroupId = organization ^. orgGroupId
  , _bwsdtoArtifactId = branch ^. bArtifactId
  , _bwsdtoParentPackageId = branch ^. bParentPackageId
  , _bwsdtoLastAppliedParentPackageId = branch ^. bLastAppliedParentPackageId
  , _bwsdtoState = state
  }

fromDTO :: BranchDTO -> Branch
fromDTO dto =
  Branch
  { _bUuid = dto ^. bdtoUuid
  , _bName = dto ^. bdtoName
  , _bArtifactId = dto ^. bdtoArtifactId
  , _bParentPackageId = dto ^. bdtoParentPackageId
  , _bLastAppliedParentPackageId = dto ^. bdtoLastAppliedParentPackageId
  , _bLastMergeCheckpointPackageId = Nothing
  }
