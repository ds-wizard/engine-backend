module Wizard.Api.Resource.Branch.BranchStateSM where

import Data.Swagger

import Wizard.Api.Resource.Branch.BranchStateJM ()
import Wizard.Model.Branch.BranchState

instance ToSchema BranchState
