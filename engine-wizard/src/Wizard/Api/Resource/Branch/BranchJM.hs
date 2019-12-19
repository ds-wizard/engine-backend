module Wizard.Api.Resource.Branch.BranchJM where

import Data.Aeson

import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchStateJM ()
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON BranchDTO where
  parseJSON = simpleParseJSON "_branchDTO"

instance ToJSON BranchDTO where
  toJSON = simpleToJSON "_branchDTO"
