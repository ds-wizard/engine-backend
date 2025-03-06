module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires

instance ToSchema QuestionnaireVersionRevertDTO where
  declareNamedSchema = toSwagger (questionnaireVersion1RevertDto questionnaire1Uuid)
