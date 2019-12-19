module Wizard.Api.Resource.Branch.BranchChangeJM where

import Data.Aeson

import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Event.EventJM ()
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON BranchChangeDTO where
  parseJSON = simpleParseJSON "_branchChangeDTO"

instance ToJSON BranchChangeDTO where
  toJSON = simpleToJSON "_branchChangeDTO"
