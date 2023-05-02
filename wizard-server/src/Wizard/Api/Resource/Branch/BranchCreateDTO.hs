module Wizard.Api.Resource.Branch.BranchCreateDTO where

import GHC.Generics

data BranchCreateDTO = BranchCreateDTO
  { name :: String
  , kmId :: String
  , version :: String
  , previousPackageId :: Maybe String
  }
  deriving (Generic)
