module Wizard.Api.Resource.Websocket.QuestionnaireActionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeJM ()
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Api.Resource.Questionnaire.File.QuestionnaireFileSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()
import Wizard.Api.Resource.User.OnlineUserInfoJM ()
import Wizard.Api.Resource.Websocket.QuestionnaireActionDTO

instance FromJSON ClientQuestionnaireActionDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ClientQuestionnaireActionDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")

instance FromJSON ServerQuestionnaireActionDTO where
  parseJSON = genericParseJSON (jsonOptionsWithTypeField "type")

instance ToJSON ServerQuestionnaireActionDTO where
  toJSON = genericToJSON (jsonOptionsWithTypeField "type")
