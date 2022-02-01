module Wizard.Api.Resource.Branch.BranchChangeDTO where

import GHC.Generics

data BranchChangeDTO =
  BranchChangeDTO
    { _branchChangeDTOName :: String
    , _branchChangeDTOKmId :: String
    }
  deriving (Generic)
