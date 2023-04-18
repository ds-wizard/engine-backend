module Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireSuggestionJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.QuestionnaireSuggestion
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnaireSuggestion where
  declareNamedSchema = toSwagger (toSuggestion questionnaire1)
