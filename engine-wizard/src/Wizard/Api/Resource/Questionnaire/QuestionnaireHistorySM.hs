module Wizard.Api.Resource.Questionnaire.QuestionnaireHistorySM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireHistoryDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireHistoryJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionSM ()
import Wizard.Service.Questionnaire.QuestionnaireMapper

instance ToSchema QuestionnaireHistoryDTO where
  declareNamedSchema = simpleToSchema (toHistoryDTO [] [])
