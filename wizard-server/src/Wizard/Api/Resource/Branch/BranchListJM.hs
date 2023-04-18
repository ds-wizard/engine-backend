module Wizard.Api.Resource.Branch.BranchListJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Branch.BranchStateJM ()
import Wizard.Model.Branch.BranchList

instance FromJSON BranchList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BranchList where
  toJSON = genericToJSON jsonOptions
