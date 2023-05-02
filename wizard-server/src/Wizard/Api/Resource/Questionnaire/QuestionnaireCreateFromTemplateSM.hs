module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnaireCreateFromTemplateDTO where
  declareNamedSchema = toSwagger (toCreateFromTemplateDTO questionnaire1)
