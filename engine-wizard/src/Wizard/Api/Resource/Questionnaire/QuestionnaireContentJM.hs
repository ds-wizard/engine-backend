module Wizard.Api.Resource.Questionnaire.QuestionnaireContentJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()

instance FromJSON QuestionnaireContentDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireContentDTO where
  toJSON = genericToJSON jsonOptions
