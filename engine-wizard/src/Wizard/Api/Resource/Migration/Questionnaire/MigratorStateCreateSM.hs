module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateJM ()
import Wizard.Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates

instance ToSchema MigratorStateCreateDTO where
  declareNamedSchema = simpleToSchema migratorStateCreate
