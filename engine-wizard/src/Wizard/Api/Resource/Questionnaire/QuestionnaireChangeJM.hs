module Wizard.Api.Resource.Questionnaire.QuestionnaireChangeJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Questionnaire.QuestionnaireAccessibilityJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()

instance FromJSON QuestionnaireChangeDTO where
  parseJSON = simpleParseJSON "_questionnaireChangeDTO"

instance ToJSON QuestionnaireChangeDTO where
  toJSON = simpleToJSON "_questionnaireChangeDTO"
