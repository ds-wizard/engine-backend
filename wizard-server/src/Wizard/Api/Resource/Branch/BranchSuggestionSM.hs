module Wizard.Api.Resource.Branch.BranchSuggestionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Branch.BranchSuggestionJM ()
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Model.Branch.BranchSuggestion

instance ToSchema BranchSuggestion where
  declareNamedSchema = toSwagger amsterdamBranchSuggestion
