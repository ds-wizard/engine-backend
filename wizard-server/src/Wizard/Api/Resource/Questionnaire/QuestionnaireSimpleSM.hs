module Wizard.Api.Resource.Questionnaire.QuestionnaireSimpleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireSimpleJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.QuestionnaireSimple
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnaireSimple where
  declareNamedSchema = toSwagger (toSimple questionnaire1)
