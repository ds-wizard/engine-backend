module Wizard.Api.Resource.Questionnaire.QuestionnaireLabelSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema LabelDTO where
  declareNamedSchema = simpleToSchema (toLabelDTO . head $ fLabels)
