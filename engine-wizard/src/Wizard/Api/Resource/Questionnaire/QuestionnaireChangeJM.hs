module Wizard.Api.Resource.Questionnaire.QuestionnaireChangeJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()

instance FromJSON QuestionnaireChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireChangeDTO where
  toJSON = genericToJSON simpleOptions
