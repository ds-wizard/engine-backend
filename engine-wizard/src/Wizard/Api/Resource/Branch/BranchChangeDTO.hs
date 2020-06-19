module Wizard.Api.Resource.Branch.BranchChangeDTO where

import GHC.Generics

import Shared.Model.Event.Event

data BranchChangeDTO =
  BranchChangeDTO
    { _branchChangeDTOName :: String
    , _branchChangeDTOKmId :: String
    , _branchChangeDTOEvents :: [Event]
    }
  deriving (Generic)
