module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailSM ()
import Wizard.Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates

instance ToSchema MigratorStateDTO where
  declareNamedSchema = simpleToSchema nlQtnMigrationStateDto
