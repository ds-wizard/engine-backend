module Api.Resource.Questionnaire.QuestionnaireChangeJM where

import Data.Aeson

import Api.Resource.Questionnaire.QuestionnaireAccessibilityJM ()
import Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Api.Resource.Questionnaire.QuestionnaireLabelJM ()
import Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON QuestionnaireChangeDTO where
  parseJSON = simpleParseJSON "_questionnaireChangeDTO"

instance ToJSON QuestionnaireChangeDTO where
  toJSON = simpleToJSON "_questionnaireChangeDTO"
