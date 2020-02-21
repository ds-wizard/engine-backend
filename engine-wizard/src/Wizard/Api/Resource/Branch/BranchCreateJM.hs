module Wizard.Api.Resource.Branch.BranchCreateJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Branch.BranchCreateDTO

instance FromJSON BranchCreateDTO where
  parseJSON = simpleParseJSON "_branchCreateDTO"

instance ToJSON BranchCreateDTO where
  toJSON = simpleToJSON "_branchCreateDTO"
