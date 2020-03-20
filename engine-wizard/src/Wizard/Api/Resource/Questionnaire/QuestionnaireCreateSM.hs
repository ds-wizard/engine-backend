module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireAccessibilitySM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires

instance ToSchema QuestionnaireCreateDTO where
  declareNamedSchema = simpleToSchema "_questionnaireChangeDTO" questionnaire1Create
