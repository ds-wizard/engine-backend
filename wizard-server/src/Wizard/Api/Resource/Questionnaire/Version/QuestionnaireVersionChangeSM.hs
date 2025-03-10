module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires

instance ToSchema QuestionnaireVersionChangeDTO where
  declareNamedSchema = toSwagger (questionnaireVersion2ChangeDto questionnaire1Uuid)
