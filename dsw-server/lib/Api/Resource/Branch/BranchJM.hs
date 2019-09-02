module Api.Resource.Branch.BranchJM where

import Data.Aeson

import Api.Resource.Branch.BranchDTO
import Api.Resource.Branch.BranchStateJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON BranchDTO where
  parseJSON = simpleParseJSON "_branchDTO"

instance ToJSON BranchDTO where
  toJSON = simpleToJSON "_branchDTO"
