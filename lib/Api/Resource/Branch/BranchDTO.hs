module Api.Resource.Branch.BranchDTO where

import Control.Monad
import Data.Aeson
import Data.UUID

data BranchDTO = BranchDTO
  { _branchDTOUuid :: UUID
  , _branchDTOName :: String
  , _branchDTOOrganizationId :: String
  , _branchDTOKmId :: String
  , _branchDTOParentPackageId :: Maybe String
  , _branchDTOLastAppliedParentPackageId :: Maybe String
  }

instance FromJSON BranchDTO where
  parseJSON (Object o) = do
    _branchDTOUuid <- o .: "uuid"
    _branchDTOName <- o .: "name"
    _branchDTOOrganizationId <- o .: "organizationId"
    _branchDTOKmId <- o .: "kmId"
    _branchDTOParentPackageId <- o .: "parentPackageId"
    _branchDTOLastAppliedParentPackageId <- o .: "lastAppliedParentPackageId"
    return BranchDTO {..}
  parseJSON _ = mzero

instance ToJSON BranchDTO where
  toJSON BranchDTO {..} =
    object
      [ "uuid" .= _branchDTOUuid
      , "name" .= _branchDTOName
      , "organizationId" .= _branchDTOOrganizationId
      , "kmId" .= _branchDTOKmId
      , "parentPackageId" .= _branchDTOParentPackageId
      , "lastAppliedParentPackageId" .= _branchDTOLastAppliedParentPackageId
      ]
