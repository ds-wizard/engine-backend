module Wizard.Api.Resource.Branch.BranchChangeDTO where

import GHC.Generics

import Wizard.Api.Resource.Event.EventDTO

data BranchChangeDTO =
  BranchChangeDTO
    { _branchChangeDTOName :: String
    , _branchChangeDTOKmId :: String
    , _branchChangeDTOEvents :: [EventDTO]
    }
  deriving (Generic)
