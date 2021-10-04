module Wizard.Api.Resource.Admin.AdminExecutionResultSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Admin.AdminExecutionResultDTO
import Wizard.Api.Resource.Admin.AdminExecutionResultJM ()
import Wizard.Database.Migration.Development.Admin.Data.Admins

instance ToSchema AdminExecutionResultDTO where
  declareNamedSchema = simpleToSchema execution1Result
