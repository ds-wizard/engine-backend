module Wizard.Api.Resource.Branch.BranchWithEventsJM where

import Data.Aeson

import Wizard.Api.Resource.Branch.BranchWithEventsDTO
import Wizard.Api.Resource.Event.EventJM ()
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON BranchWithEventsDTO where
  parseJSON = simpleParseJSON "_branchWithEventsDTO"

instance ToJSON BranchWithEventsDTO where
  toJSON = simpleToJSON "_branchWithEventsDTO"
