module Wizard.Api.Resource.Branch.BranchChangeSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Branch.BranchChangeDTO
import Wizard.Api.Resource.Branch.BranchChangeJM ()
import Wizard.Database.Migration.Development.Branch.Data.Branches

instance ToSchema BranchChangeDTO where
  declareNamedSchema = toSwagger amsterdamBranchChange
