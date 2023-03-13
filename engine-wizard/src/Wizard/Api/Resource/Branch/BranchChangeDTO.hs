module Wizard.Api.Resource.Branch.BranchChangeDTO where

import GHC.Generics

data BranchChangeDTO = BranchChangeDTO
  { name :: String
  , kmId :: String
  , version :: String
  , description :: String
  , readme :: String
  , license :: String
  }
  deriving (Generic)
