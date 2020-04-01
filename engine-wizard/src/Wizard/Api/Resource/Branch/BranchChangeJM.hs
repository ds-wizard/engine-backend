module Wizard.Api.Resource.Branch.BranchChangeJM where

import Data.Aeson

import Shared.Api.Resource.Event.EventJM ()
import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Branch.BranchChangeDTO

instance FromJSON BranchChangeDTO where
  parseJSON = simpleParseJSON "_branchChangeDTO"

instance ToJSON BranchChangeDTO where
  toJSON = simpleToJSON "_branchChangeDTO"
