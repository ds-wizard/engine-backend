module Wizard.Api.Resource.Questionnaire.QuestionnaireChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnairePermChangeJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()

instance FromJSON QuestionnaireChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireChangeDTO where
  toJSON = genericToJSON jsonOptions
