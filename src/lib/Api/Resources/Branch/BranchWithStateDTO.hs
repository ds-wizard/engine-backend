module Api.Resources.Branch.BranchWithStateDTO where

import Control.Lens (makeLenses, (^.))
import Control.Monad
import Data.Aeson
import Data.Text
import Data.UUID

import Common.Types
import Common.Uuid

import Model.Branch.BranchState

data BranchWithStateDTO = BranchWithStateDTO
  { _bwsdtoUuid :: UUID
  , _bwsdtoName :: String
  , _bwsdtoArtifactId :: String
  , _bwsdtoParentPackageId :: Maybe String
  , _bwsdtoState :: BranchState
  }

makeLenses ''BranchWithStateDTO

instance FromJSON BranchState

instance ToJSON BranchState

instance FromJSON BranchWithStateDTO where
  parseJSON (Object o) = do
    _bwsdtoUuid <- o .: "uuid"
    _bwsdtoName <- o .: "name"
    _bwsdtoArtifactId <- o .: "artifactId"
    _bwsdtoParentPackageId <- o .: "parentPackageId"
    _bwsdtoState <- o .: "state"
    return BranchWithStateDTO {..}
  parseJSON _ = mzero

instance ToJSON BranchWithStateDTO where
  toJSON BranchWithStateDTO {..} =
    object
      [ "uuid" .= _bwsdtoUuid
      , "name" .= _bwsdtoName
      , "artifactId" .= _bwsdtoArtifactId
      , "parentPackageId" .= _bwsdtoParentPackageId
      , "state" .= _bwsdtoState
      ]
