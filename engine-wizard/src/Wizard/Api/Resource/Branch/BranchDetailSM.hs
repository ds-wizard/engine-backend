module Wizard.Api.Resource.Branch.BranchDetailSM where

import Data.Swagger

import Shared.Api.Resource.Event.EventSM ()
import Shared.Util.Swagger
import Wizard.Api.Resource.Branch.BranchDetailDTO
import Wizard.Api.Resource.Branch.BranchDetailJM ()
import Wizard.Api.Resource.Branch.BranchStateSM ()
import Wizard.Database.Migration.Development.Branch.Data.Branches

instance ToSchema BranchDetailDTO where
  declareNamedSchema = simpleToSchema amsterdamBranchDetail
