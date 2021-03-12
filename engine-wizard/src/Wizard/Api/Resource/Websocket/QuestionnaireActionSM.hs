module Wizard.Api.Resource.Websocket.QuestionnaireActionSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeSM ()
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventSM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplySM ()
import Wizard.Api.Resource.User.OnlineUserInfoSM ()
import Wizard.Api.Resource.Websocket.QuestionnaireActionDTO
import Wizard.Api.Resource.Websocket.QuestionnaireActionJM ()
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireActions

instance ToSchema ClientQuestionnaireActionDTO where
  declareNamedSchema = simpleToSchema ensureOnlineUserAction

instance ToSchema ServerQuestionnaireActionDTO where
  declareNamedSchema = simpleToSchema setUserListAction
