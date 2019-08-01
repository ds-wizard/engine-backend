module Api.Resource.Branch.BranchChangeDTO where

import GHC.Generics

import Api.Resource.Event.EventDTO

data BranchChangeDTO = BranchChangeDTO
  { _branchChangeDTOName :: String
  , _branchChangeDTOKmId :: String
  , _branchChangeDTOEvents :: [EventDTO]
  } deriving (Generic)
