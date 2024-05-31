module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()

instance FromJSON QuestionnaireDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireDetailDTO where
  toJSON = genericToJSON jsonOptions
