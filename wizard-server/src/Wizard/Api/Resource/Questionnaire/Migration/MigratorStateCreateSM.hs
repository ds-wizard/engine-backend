module Wizard.Api.Resource.Questionnaire.Migration.MigratorStateCreateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateCreateDTO
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateCreateJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.MigratorStates

instance ToSchema MigratorStateCreateDTO where
  declareNamedSchema = toSwagger migratorStateCreate
