module Api.Resource.Branch.BranchWithEventsJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Branch.BranchWithEventsDTO
import Api.Resource.Event.EventDTO ()

instance FromJSON BranchWithEventsDTO where
  parseJSON (Object o) = do
    _branchWithEventsDTOUuid <- o .: "uuid"
    _branchWithEventsDTOName <- o .: "name"
    _branchWithEventsDTOKmId <- o .: "kmId"
    _branchWithEventsDTOMetamodelVersion <- o .: "metamodelVersion"
    _branchWithEventsDTOParentPackageId <- o .: "parentPackageId"
    _branchWithEventsDTOLastAppliedParentPackageId <- o .: "lastAppliedParentPackageId"
    _branchWithEventsDTOLastMergeCheckpointPackageId <- o .: "lastMergeCheckpointPackageId"
    _branchWithEventsDTOEvents <- o .: "events"
    _branchWithEventsDTOOwnerUuid <- o .: "ownerUuid"
    _branchWithEventsDTOCreatedAt <- o .: "createdAt"
    _branchWithEventsDTOUpdatedAt <- o .: "updatedAt"
    return BranchWithEventsDTO {..}
  parseJSON _ = mzero

instance ToJSON BranchWithEventsDTO where
  toJSON BranchWithEventsDTO {..} =
    object
      [ "uuid" .= _branchWithEventsDTOUuid
      , "name" .= _branchWithEventsDTOName
      , "kmId" .= _branchWithEventsDTOKmId
      , "metamodelVersion" .= _branchWithEventsDTOMetamodelVersion
      , "parentPackageId" .= _branchWithEventsDTOParentPackageId
      , "lastAppliedParentPackageId" .= _branchWithEventsDTOLastAppliedParentPackageId
      , "lastMergeCheckpointPackageId" .= _branchWithEventsDTOLastMergeCheckpointPackageId
      , "events" .= _branchWithEventsDTOEvents
      , "ownerUuid" .= _branchWithEventsDTOOwnerUuid
      , "createdAt" .= _branchWithEventsDTOCreatedAt
      , "updatedAt" .= _branchWithEventsDTOUpdatedAt
      ]
