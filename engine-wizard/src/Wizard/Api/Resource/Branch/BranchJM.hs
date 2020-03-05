module Wizard.Api.Resource.Branch.BranchJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchStateJM ()

instance FromJSON BranchDTO where
  parseJSON = simpleParseJSON "_branchDTO"

instance ToJSON BranchDTO where
  toJSON = simpleToJSON "_branchDTO"
