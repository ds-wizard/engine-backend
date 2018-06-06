module Api.Resource.Branch.BranchWithStateDTO where

import Control.Monad
import Data.Aeson
import Data.UUID

import Model.Branch.BranchState

data BranchWithStateDTO = BranchWithStateDTO
  { _branchWithStateDTOUuid :: UUID
  , _branchWithStateDTOName :: String
  , _branchWithStateDTOOrganizationId :: String
  , _branchWithStateDTOKmId :: String
  , _branchWithStateDTOParentPackageId :: Maybe String
  , _branchWithStateDTOState :: BranchState
  , _branchWithStateDTOLastAppliedParentPackageId :: Maybe String
  }

instance FromJSON BranchWithStateDTO where
  parseJSON (Object o) = do
    _branchWithStateDTOUuid <- o .: "uuid"
    _branchWithStateDTOName <- o .: "name"
    _branchWithStateDTOOrganizationId <- o .: "organizationId"
    _branchWithStateDTOKmId <- o .: "kmId"
    _branchWithStateDTOParentPackageId <- o .: "parentPackageId"
    _branchWithStateDTOLastAppliedParentPackageId <- o .: "lastAppliedParentPackageId"
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
      , "stateType" .=
        case _branchWithStateDTOState of
          BSDefault -> "Default"
          BSEdited -> "Edited"
          BSOutdated -> "Outdated"
          BSMigrating -> "Migrating"
          BSMigrated -> "Migrated"
      ]
