module Api.Resource.Branch.BranchJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Branch.BranchDTO
import Api.Resource.Common

instance FromJSON BranchDTO where
  parseJSON (Object o) = do
    _branchDTOUuid <- o .: "uuid"
    _branchDTOName <- o .: "name"
    _branchDTOOrganizationId <- o .: "organizationId"
    _branchDTOKmId <- o .: "kmId"
    _branchDTOParentPackageId <- o .: "parentPackageId"
    _branchDTOLastAppliedParentPackageId <- o .: "lastAppliedParentPackageId"
    _branchDTOOwnerUuid <- o .: "ownerUuid"
    _branchDTOCreatedAt <- o .: "createdAt"
    _branchDTOUpdatedAt <- o .: "updatedAt"
    stateType <- o .: "stateType"
    case deserializeBranchState stateType of
      (Just _branchDTOState) -> return BranchDTO {..}
      Nothing -> fail "Unsupported state"
  parseJSON _ = mzero

instance ToJSON BranchDTO where
  toJSON BranchDTO {..} =
    object
      [ "uuid" .= _branchDTOUuid
      , "name" .= _branchDTOName
      , "organizationId" .= _branchDTOOrganizationId
      , "kmId" .= _branchDTOKmId
      , "stateType" .= serializeBranchState _branchDTOState
      , "parentPackageId" .= _branchDTOParentPackageId
      , "lastAppliedParentPackageId" .= _branchDTOLastAppliedParentPackageId
      , "ownerUuid" .= _branchDTOOwnerUuid
      , "createdAt" .= _branchDTOCreatedAt
      , "updatedAt" .= _branchDTOUpdatedAt
      ]
