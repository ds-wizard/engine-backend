module Wizard.Api.Resource.Branch.BranchListSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Branch.BranchListJM ()
import Wizard.Api.Resource.Branch.BranchStateSM ()
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Model.Branch.BranchList

instance ToSchema BranchList where
  declareNamedSchema = simpleToSchema' "_branchList" amsterdamBranchList
