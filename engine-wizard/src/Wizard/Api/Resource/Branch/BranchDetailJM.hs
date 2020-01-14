module Wizard.Api.Resource.Branch.BranchDetailJM where

import Data.Aeson

import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchStateJM ()
import Wizard.Api.Resource.Event.EventJM ()
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON BranchDetailDTO where
  parseJSON = simpleParseJSON "_branchDetailDTO"

instance ToJSON BranchDetailDTO where
  toJSON = simpleToJSON "_branchDetailDTO"
