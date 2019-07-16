module Api.Resource.Branch.BranchWithEventsJM where

import Data.Aeson

import Api.Resource.Branch.BranchWithEventsDTO
import Api.Resource.Event.EventJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON BranchWithEventsDTO where
  parseJSON = simpleParseJSON "_branchWithEventsDTO"

instance ToJSON BranchWithEventsDTO where
  toJSON = simpleToJSON "_branchWithEventsDTO"
