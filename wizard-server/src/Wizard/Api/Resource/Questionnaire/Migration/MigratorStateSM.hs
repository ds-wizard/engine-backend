module Wizard.Api.Resource.Questionnaire.Migration.MigratorStateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireSM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.MigratorStates

instance ToSchema MigratorStateDTO where
  declareNamedSchema = toSwagger nlQtnMigrationStateDto
