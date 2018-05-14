module Api.Resource.Branch.BranchWithStateDTO where

import Control.Lens (makeLenses)
import Control.Monad
import Data.Aeson
import Data.UUID

import Model.Branch.BranchState

data BranchWithStateDTO = BranchWithStateDTO
  { _bwsdtoUuid :: UUID
  , _bwsdtoName :: String
  , _bwsdtoOrganizationId :: String
  , _bwsdtoKmId :: String
  , _bwsdtoParentPackageId :: Maybe String
  , _bwsdtoState :: BranchState
  , _bwsdtoLastAppliedParentPackageId :: Maybe String
  }

makeLenses ''BranchWithStateDTO

instance FromJSON BranchWithStateDTO where
  parseJSON (Object o) = do
    _bwsdtoUuid <- o .: "uuid"
    _bwsdtoName <- o .: "name"
    _bwsdtoOrganizationId <- o .: "organizationId"
    _bwsdtoKmId <- o .: "kmId"
    _bwsdtoParentPackageId <- o .: "parentPackageId"
    _bwsdtoLastAppliedParentPackageId <- o .: "lastAppliedParentPackageId"
    stateType <- o .: "stateType"
    case getState stateType of
      (Just _bwsdtoState) -> return BranchWithStateDTO {..}
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
      [ "uuid" .= _bwsdtoUuid
      , "name" .= _bwsdtoName
      , "organizationId" .= _bwsdtoOrganizationId
      , "kmId" .= _bwsdtoKmId
      , "parentPackageId" .= _bwsdtoParentPackageId
      , "lastAppliedParentPackageId" .= _bwsdtoLastAppliedParentPackageId
      , "stateType" .=
        case _bwsdtoState of
          BSDefault -> "Default"
          BSEdited -> "Edited"
          BSOutdated -> "Outdated"
          BSMigrating -> "Migrating"
          BSMigrated -> "Migrated"
      ]
