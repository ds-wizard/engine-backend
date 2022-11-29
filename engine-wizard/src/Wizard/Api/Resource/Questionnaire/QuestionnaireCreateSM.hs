module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires

instance ToSchema QuestionnaireCreateDTO where
  declareNamedSchema = toSwagger questionnaire1Create
