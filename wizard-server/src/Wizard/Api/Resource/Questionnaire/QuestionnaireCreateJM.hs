module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()

instance FromJSON QuestionnaireCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireCreateDTO where
  toJSON = genericToJSON jsonOptions
