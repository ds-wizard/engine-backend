module Wizard.Api.Resource.Branch.BranchListJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Branch.BranchStateJM ()
import Wizard.Model.Branch.BranchList

instance FromJSON BranchList where
  parseJSON = simpleParseJSON "_branchList"

instance ToJSON BranchList where
  toJSON = simpleToJSON "_branchList"
