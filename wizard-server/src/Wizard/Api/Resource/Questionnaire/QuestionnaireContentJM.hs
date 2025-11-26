module Wizard.Api.Resource.Questionnaire.QuestionnaireContentJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventListJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentThreadListJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionListJM ()

instance FromJSON QuestionnaireContentDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON QuestionnaireContentDTO where
  toJSON = genericToJSON jsonOptions
