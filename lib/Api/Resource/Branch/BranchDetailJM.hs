module Api.Resource.Branch.BranchDetailJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Branch.BranchDetailDTO
import Api.Resource.Common
import Api.Resource.Event.EventJM ()

instance FromJSON BranchDetailDTO where
  parseJSON (Object o) = do
    _branchDetailDTOUuid <- o .: "uuid"
    _branchDetailDTOName <- o .: "name"
    _branchDetailDTOOrganizationId <- o .: "organizationId"
    _branchDetailDTOKmId <- o .: "kmId"
    _branchDetailDTOParentPackageId <- o .: "parentPackageId"
    _branchDetailDTOLastAppliedParentPackageId <- o .: "lastAppliedParentPackageId"
    _branchDetailDTOOwnerUuid <- o .: "ownerUuid"
    _branchDetailDTOEvents <- o .: "events"
    _branchDetailDTOCreatedAt <- o .: "createdAt"
    _branchDetailDTOUpdatedAt <- o .: "updatedAt"
    stateType <- o .: "stateType"
    case deserializeBranchState stateType of
      (Just _branchDetailDTOState) -> return BranchDetailDTO {..}
      Nothing -> fail "Unsupported state"
  parseJSON _ = mzero

instance ToJSON BranchDetailDTO where
  toJSON BranchDetailDTO {..} =
    object
      [ "uuid" .= _branchDetailDTOUuid
      , "name" .= _branchDetailDTOName
      , "organizationId" .= _branchDetailDTOOrganizationId
      , "kmId" .= _branchDetailDTOKmId
      , "stateType" .= serializeBranchState _branchDetailDTOState
      , "parentPackageId" .= _branchDetailDTOParentPackageId
      , "lastAppliedParentPackageId" .= _branchDetailDTOLastAppliedParentPackageId
      , "ownerUuid" .= _branchDetailDTOOwnerUuid
      , "events" .= _branchDetailDTOEvents
      , "createdAt" .= _branchDetailDTOCreatedAt
      , "updatedAt" .= _branchDetailDTOUpdatedAt
      ]
