module Api.Resources.Branch.BranchDTO where

import Control.Lens (makeLenses, (^.))
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Common.Types
import Common.Uuid

data BranchDTO = BranchDTO
  { _bdtoUuid :: UUID
  , _bdtoName :: String
  , _bdtoArtifactId :: String
  , _bdtoParentPackageId :: Maybe String
  }

makeLenses ''BranchDTO

instance FromJSON BranchDTO where
  parseJSON (Object o) = do
    _bdtoUuid <- o .: "uuid"
    _bdtoName <- o .: "name"
    _bdtoArtifactId <- o .: "artifactId"
    _bdtoParentPackageId <- o .: "parentPackageId"
    return BranchDTO {..}
  parseJSON _ = mzero

instance ToJSON BranchDTO where
  toJSON BranchDTO {..} =
    object
      [ "uuid" .= _bdtoUuid
      , "name" .= _bdtoName
      , "artifactId" .= _bdtoArtifactId
      , "parentPackageId" .= _bdtoParentPackageId
      ]
