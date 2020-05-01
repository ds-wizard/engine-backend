module Wizard.Api.Resource.Questionnaire.QuestionnaireChangeJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireAccessibilityJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()

instance FromJSON QuestionnaireChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireChangeDTO where
  toJSON = genericToJSON simpleOptions
