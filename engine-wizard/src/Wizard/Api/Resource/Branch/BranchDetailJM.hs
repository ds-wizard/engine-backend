module Wizard.Api.Resource.Branch.BranchDetailJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchStateJM ()
import Wizard.Api.Resource.Event.EventJM ()

instance FromJSON BranchDetailDTO where
  parseJSON = simpleParseJSON "_branchDetailDTO"

instance ToJSON BranchDetailDTO where
  toJSON = simpleToJSON "_branchDetailDTO"
