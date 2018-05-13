module Api.Resource.Branch.BranchDTO where

import Control.Lens (makeLenses)
import Control.Monad
import Data.Aeson
import Data.UUID

data BranchDTO = BranchDTO
  { _bdtoUuid :: UUID
  , _bdtoName :: String
  , _bdtoOrganizationId :: String
  , _bdtoArtifactId :: String
  , _bdtoParentPackageId :: Maybe String
  , _bdtoLastAppliedParentPackageId :: Maybe String
  }

makeLenses ''BranchDTO

instance FromJSON BranchDTO where
  parseJSON (Object o) = do
    _bdtoUuid <- o .: "uuid"
    _bdtoName <- o .: "name"
    _bdtoOrganizationId <- o .: "organizationId"
    _bdtoArtifactId <- o .: "artifactId"
    _bdtoParentPackageId <- o .: "parentPackageId"
    _bdtoLastAppliedParentPackageId <- o .: "lastAppliedParentPackageId"
    return BranchDTO {..}
  parseJSON _ = mzero

instance ToJSON BranchDTO where
  toJSON BranchDTO {..} =
    object
      [ "uuid" .= _bdtoUuid
      , "name" .= _bdtoName
      , "organizationId" .= _bdtoOrganizationId
      , "artifactId" .= _bdtoArtifactId
      , "parentPackageId" .= _bdtoParentPackageId
      , "lastAppliedParentPackageId" .= _bdtoLastAppliedParentPackageId
      ]
