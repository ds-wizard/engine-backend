module Wizard.Api.Resource.Admin.AdminExecutionSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Admin.AdminExecutionDTO
import Wizard.Api.Resource.Admin.AdminExecutionJM ()
import Wizard.Database.Migration.Development.Admin.Data.Admins

instance ToSchema AdminExecutionDTO where
  declareNamedSchema = simpleToSchema execution1
