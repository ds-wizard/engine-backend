module Api.Resource.Branch.BranchChangeJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Branch.BranchChangeDTO

instance FromJSON BranchChangeDTO where
  parseJSON (Object o) = do
    _branchChangeDTOName <- o .: "name"
    _branchChangeDTOKmId <- o .: "kmId"
    _branchChangeDTOParentPackageId <- o .: "parentPackageId"
    return BranchChangeDTO {..}
  parseJSON _ = mzero

instance ToJSON BranchChangeDTO where
  toJSON BranchChangeDTO {..} =
    object
      [ "name" .= _branchChangeDTOName
      , "kmId" .= _branchChangeDTOKmId
      , "parentPackageId" .= _branchChangeDTOParentPackageId
      ]
