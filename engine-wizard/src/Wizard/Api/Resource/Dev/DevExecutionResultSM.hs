module Wizard.Api.Resource.Dev.DevExecutionResultSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Dev.DevExecutionResultDTO
import Wizard.Api.Resource.Dev.DevExecutionResultJM ()
import Wizard.Database.Migration.Development.Dev.Data.Devs

instance ToSchema AdminExecutionResultDTO where
  declareNamedSchema = toSwagger execution1Result
