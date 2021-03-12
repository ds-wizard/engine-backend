module Wizard.Api.Resource.Websocket.QuestionnaireActionJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeJM ()
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.User.OnlineUserInfoJM ()
import Wizard.Api.Resource.Websocket.QuestionnaireActionDTO

instance FromJSON ClientQuestionnaireActionDTO where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON ClientQuestionnaireActionDTO where
  toJSON = genericToJSON simpleOptions'''

instance FromJSON ServerQuestionnaireActionDTO where
  parseJSON = genericParseJSON simpleOptions'''

instance ToJSON ServerQuestionnaireActionDTO where
  toJSON = genericToJSON simpleOptions'''
