module Service.Branch.BranchMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resources.Branch.BranchDTO
import Api.Resources.Branch.BranchWithStateDTO
import Common.Types
import Model.Branch.Branch
import Model.Branch.BranchState

toDTO :: Branch -> BranchDTO
toDTO branch =
  BranchDTO
  { _bdtoUuid = branch ^. bUuid
  , _bdtoName = branch ^. bName
  , _bdtoArtifactId = branch ^. bArtifactId
  , _bdtoParentPackageId = branch ^. bParentPackageId
  }

toWithStateDTO :: Branch -> BranchState -> BranchWithStateDTO
toWithStateDTO branch state =
  BranchWithStateDTO
  { _bwsdtoUuid = branch ^. bUuid
  , _bwsdtoName = branch ^. bName
  , _bwsdtoArtifactId = branch ^. bArtifactId
  , _bwsdtoParentPackageId = branch ^. bParentPackageId
  , _bwsdtoState = state
  }

fromDTO :: BranchDTO -> Branch
fromDTO dto =
  Branch
  { _bUuid = dto ^. bdtoUuid
  , _bName = dto ^. bdtoName
  , _bArtifactId = dto ^. bdtoArtifactId
  , _bParentPackageId = dto ^. bdtoParentPackageId
  , _bLastAppliedParentPackageId = Nothing
  , _bLastMergeCheckpointPackageId = Nothing
  }
