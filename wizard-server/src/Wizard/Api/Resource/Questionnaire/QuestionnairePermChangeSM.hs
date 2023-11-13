module Wizard.Api.Resource.Questionnaire.QuestionnairePermChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnairePermChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermChangeJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnairePermSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilitySM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnairePermChangeDTO where
  declareNamedSchema = toSwagger (toQuestionnairePermChangeDTO qtn1AlbertEditQtnPerm)
