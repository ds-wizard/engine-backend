module Wizard.Api.Resource.Questionnaire.QuestionnaireSimpleSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireSimpleJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.QuestionnaireSimple

instance ToSchema QuestionnaireSimple where
  declareNamedSchema = toSwagger questionnaire1Simple
