module Wizard.Api.Resource.Branch.BranchWithEventsJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Branch.BranchWithEventsDTO
import Wizard.Api.Resource.Event.EventJM ()

instance FromJSON BranchWithEventsDTO where
  parseJSON = simpleParseJSON "_branchWithEventsDTO"

instance ToJSON BranchWithEventsDTO where
  toJSON = simpleToJSON "_branchWithEventsDTO"
