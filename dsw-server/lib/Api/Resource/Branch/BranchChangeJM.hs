module Api.Resource.Branch.BranchChangeJM where

import Data.Aeson

import Api.Resource.Branch.BranchChangeDTO
import Api.Resource.Event.EventJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON BranchChangeDTO where
  parseJSON = simpleParseJSON "_branchChangeDTO"

instance ToJSON BranchChangeDTO where
  toJSON = simpleToJSON "_branchChangeDTO"
