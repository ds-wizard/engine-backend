module Wizard.Api.Resource.Branch.BranchCreateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Branch.BranchCreateDTO
import Wizard.Api.Resource.Branch.BranchCreateJM ()
import Wizard.Database.Migration.Development.Branch.Data.Branches

instance ToSchema BranchCreateDTO where
  declareNamedSchema = simpleToSchema amsterdamBranchCreate
