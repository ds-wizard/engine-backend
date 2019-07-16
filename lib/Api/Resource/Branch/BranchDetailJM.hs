module Api.Resource.Branch.BranchDetailJM where

import Data.Aeson

import Api.Resource.Branch.BranchDetailDTO
import Api.Resource.Branch.BranchStateJM ()
import Api.Resource.Event.EventJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON BranchDetailDTO where
  parseJSON = simpleParseJSON "_branchDetailDTO"

instance ToJSON BranchDetailDTO where
  toJSON = simpleToJSON "_branchDetailDTO"
