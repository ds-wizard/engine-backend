module Database.Migration.Development.Branch.Data.Branches where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as U

import Api.Resource.Branch.BranchDTO
import Database.Migration.Development.Organization.Data.Organizations
import LensesConfig

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
