module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()

instance FromJSON QuestionnaireCreateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON QuestionnaireCreateDTO where
  toJSON = genericToJSON simpleOptions
