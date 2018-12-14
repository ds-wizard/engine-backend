module Database.Migration.Development.Branch.Data.Branches where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Branch.BranchChangeDTO
import Api.Resource.Branch.BranchDTO
import Api.Resource.Branch.BranchWithStateDTO
import Database.Migration.Development.Organization.Data.Organizations
import LensesConfig
import Model.Branch.BranchState

amsterdamBranch =
  BranchDTO
  { _branchDTOUuid = fromJust (U.fromString "6474b24b-262b-42b1-9451-008e8363f2b6")
  , _branchDTOName = "Amsterdam KM"
  , _branchDTOOrganizationId = org1 ^. organizationId
  , _branchDTOKmId = "amsterdam-km"
  , _branchDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
  , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
  , _branchDTOOwnerUuid = Just $ fromJust (U.fromString "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66")
  , _branchDTOCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  , _branchDTOUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

amsterdamBranchChange =
  BranchChangeDTO
  { _branchChangeDTOName = amsterdamBranch ^. name
  , _branchChangeDTOKmId = amsterdamBranch ^. kmId
  , _branchChangeDTOParentPackageId = amsterdamBranch ^. parentPackageId
  }

editedAmsterdamBranchChange =
  BranchChangeDTO
  { _branchChangeDTOName = "EDITED: " ++ amsterdamBranch ^. name
  , _branchChangeDTOKmId = amsterdamBranch ^. kmId
  , _branchChangeDTOParentPackageId = amsterdamBranch ^. parentPackageId
  }

amsterdamBranchWithState =
  BranchWithStateDTO
  { _branchWithStateDTOUuid = amsterdamBranch ^. uuid
  , _branchWithStateDTOName = amsterdamBranch ^. name
  , _branchWithStateDTOOrganizationId = amsterdamBranch ^. organizationId
  , _branchWithStateDTOKmId = amsterdamBranch ^. kmId
  , _branchWithStateDTOParentPackageId = amsterdamBranch ^. parentPackageId
  , _branchWithStateDTOLastAppliedParentPackageId = amsterdamBranch ^. lastAppliedParentPackageId
  , _branchWithStateDTOState = BSDefault
  , _branchWithStateDTOOwnerUuid = amsterdamBranch ^. ownerUuid
  , _branchWithStateDTOCreatedAt = amsterdamBranch ^. createdAt
  , _branchWithStateDTOUpdatedAt = amsterdamBranch ^. updatedAt
  }

leidenBranch =
  BranchDTO
  { _branchDTOUuid = fromJust (U.fromString "47421955-ba30-48d4-8c49-9ec47eda2cad")
  , _branchDTOName = "Leiden KM"
  , _branchDTOOrganizationId = org1 ^. organizationId
  , _branchDTOKmId = "leiden-km"
  , _branchDTOParentPackageId = Just "elixir.nl:core-nl:1.0.0"
  , _branchDTOLastAppliedParentPackageId = Just "elixir.nl:core-nl:1.0.0"
  , _branchDTOOwnerUuid = Just $ fromJust (U.fromString "ec6f8e90-2a91-49ec-aa3f-9eab2267fc66")
  , _branchDTOCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  , _branchDTOUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
  }

leidenBranchChange =
  BranchChangeDTO
  { _branchChangeDTOName = leidenBranch ^. name
  , _branchChangeDTOKmId = leidenBranch ^. kmId
  , _branchChangeDTOParentPackageId = leidenBranch ^. parentPackageId
  }
