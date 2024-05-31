module Wizard.Api.Resource.Questionnaire.QuestionnaireShareChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.QuestionnairePermChangeJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireShareChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireSharingJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireVisibilityJM ()

instance FromJSON QuestionnaireShareChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireShareChangeDTO where
  toJSON = genericToJSON jsonOptions
