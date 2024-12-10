module Wizard.Model.Branch.BranchSuggestion where

import qualified Data.UUID as U
import GHC.Generics

data BranchSuggestion = BranchSuggestion
  { uuid :: U.UUID
  , name :: String
  }
  deriving (Generic, Eq, Show)
