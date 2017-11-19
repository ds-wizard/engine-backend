module Service.Branch.BranchMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resources.Branch.BranchDTO
import Common.Types
import Model.Branch.Branch

toDTO :: Branch -> BranchDTO
toDTO branch =
  BranchDTO
  { _bdtoUuid = branch ^. bUuid
  , _bdtoName = branch ^. bName
  , _bdtoArtifactId = branch ^. bArtifactId
  , _bdtoParentPackageId = branch ^. bParentPackageId
  }

fromDTO :: BranchDTO -> Branch
fromDTO dto =
  Branch
  { _bUuid = dto ^. bdtoUuid
  , _bName = dto ^. bdtoName
  , _bArtifactId = dto ^. bdtoArtifactId
  , _bParentPackageId = dto ^. bdtoParentPackageId
  }
