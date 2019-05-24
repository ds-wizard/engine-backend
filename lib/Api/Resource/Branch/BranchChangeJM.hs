module Api.Resource.Branch.BranchChangeJM where

import Control.Monad
import Data.Aeson

import Api.Resource.Branch.BranchChangeDTO
import Api.Resource.Event.EventJM ()

instance FromJSON BranchChangeDTO where
  parseJSON (Object o) = do
    _branchChangeDTOName <- o .: "name"
    _branchChangeDTOKmId <- o .: "kmId"
    _branchChangeDTOEvents <- o .: "events"
    return BranchChangeDTO {..}
  parseJSON _ = mzero

instance ToJSON BranchChangeDTO where
  toJSON BranchChangeDTO {..} =
    object ["name" .= _branchChangeDTOName, "kmId" .= _branchChangeDTOKmId, "events" .= _branchChangeDTOEvents]
