module Api.Resource.Branch.BranchCreateJM where

import Data.Aeson

import Api.Resource.Branch.BranchCreateDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON BranchCreateDTO where
  parseJSON = simpleParseJSON "_branchCreateDTO"

instance ToJSON BranchCreateDTO where
  toJSON = simpleToJSON "_branchCreateDTO"
