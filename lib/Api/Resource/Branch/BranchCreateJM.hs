module Api.Resource.Branch.BranchCreateJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Branch.BranchCreateDTO

instance FromJSON BranchCreateDTO where
  parseJSON (Object o) = do
    _branchCreateDTOName <- o .: "name"
    _branchCreateDTOKmId <- o .: "kmId"
    _branchCreateDTOParentPackageId <- o .: "parentPackageId"
    return BranchCreateDTO {..}
  parseJSON _ = mzero

instance ToJSON BranchCreateDTO where
  toJSON BranchCreateDTO {..} =
    object
      [ "name" .= _branchCreateDTOName
      , "kmId" .= _branchCreateDTOKmId
      , "parentPackageId" .= _branchCreateDTOParentPackageId
      ]
