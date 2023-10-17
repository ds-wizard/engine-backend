module Wizard.Api.Resource.Questionnaire.QuestionnairePermChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnairePermChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()

instance FromJSON QuestionnairePermChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnairePermChangeDTO where
  toJSON = genericToJSON jsonOptions
