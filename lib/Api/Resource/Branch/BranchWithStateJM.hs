module Api.Resource.Branch.BranchWithStateJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Branch.BranchWithStateDTO
import Model.Branch.BranchState

instance FromJSON BranchWithStateDTO where
  parseJSON (Object o) = do
    _branchWithStateDTOUuid <- o .: "uuid"
    _branchWithStateDTOName <- o .: "name"
    _branchWithStateDTOOrganizationId <- o .: "organizationId"
    _branchWithStateDTOKmId <- o .: "kmId"
    _branchWithStateDTOParentPackageId <- o .: "parentPackageId"
    _branchWithStateDTOLastAppliedParentPackageId <- o .: "lastAppliedParentPackageId"
    _branchWithStateDTOOwnerUuid <- o .: "ownerUuid"
    _branchWithStateDTOCreatedAt <- o .: "createdAt"
    _branchWithStateDTOUpdatedAt <- o .: "updatedAt"
    stateType <- o .: "stateType"
    case getState stateType of
      (Just _branchWithStateDTOState) -> return BranchWithStateDTO {..}
      Nothing -> fail "Unsupported state"
    where
      getState "Default" = Just BSDefault
      getState "Edited" = Just BSEdited
      getState "Outdated" = Just BSOutdated
      getState "Migrating" = Just BSMigrating
      getState "Migrated" = Just BSMigrated
      getState _ = Nothing
  parseJSON _ = mzero

instance ToJSON BranchWithStateDTO where
  toJSON BranchWithStateDTO {..} =
    object
      [ "uuid" .= _branchWithStateDTOUuid
      , "name" .= _branchWithStateDTOName
      , "organizationId" .= _branchWithStateDTOOrganizationId
      , "kmId" .= _branchWithStateDTOKmId
      , "parentPackageId" .= _branchWithStateDTOParentPackageId
      , "lastAppliedParentPackageId" .= _branchWithStateDTOLastAppliedParentPackageId
      , "ownerUuid" .= _branchWithStateDTOOwnerUuid
      , "createdAt" .= _branchWithStateDTOCreatedAt
      , "updatedAt" .= _branchWithStateDTOUpdatedAt
      , "stateType" .=
        case _branchWithStateDTOState of
          BSDefault -> "Default"
          BSEdited -> "Edited"
          BSOutdated -> "Outdated"
          BSMigrating -> "Migrating"
          BSMigrated -> "Migrated"
      ]
