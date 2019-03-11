module Api.Resource.Branch.BranchChangeDTO where

import Api.Resource.Event.EventDTO

data BranchChangeDTO = BranchChangeDTO
  { _branchChangeDTOName :: String
  , _branchChangeDTOKmId :: String
  , _branchChangeDTOEvents :: [EventDTO]
  }
