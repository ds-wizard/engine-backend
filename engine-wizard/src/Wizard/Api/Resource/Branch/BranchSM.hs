module Wizard.Api.Resource.Branch.BranchSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Branch.BranchDTO
import Wizard.Api.Resource.Branch.BranchJM ()
import Wizard.Api.Resource.Branch.BranchStateSM ()
import Wizard.Database.Migration.Development.Branch.Data.Branches

instance ToSchema BranchDTO where
  declareNamedSchema = simpleToSchema amsterdamBranch
