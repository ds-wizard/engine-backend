module Wizard.Api.Resource.Dev.DevExecutionSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Dev.DevExecutionDTO
import Wizard.Api.Resource.Dev.DevExecutionJM ()
import Wizard.Database.Migration.Development.Dev.Data.Devs

instance ToSchema DevExecutionDTO where
  declareNamedSchema = toSwagger execution1
