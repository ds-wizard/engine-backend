module Wizard.Api.Resource.Branch.BranchChangeDTO where

import GHC.Generics

data BranchChangeDTO = BranchChangeDTO
  { name :: String
  , kmId :: String
  }
  deriving (Generic)
