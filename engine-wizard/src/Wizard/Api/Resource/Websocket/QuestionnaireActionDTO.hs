module Wizard.Api.Resource.Websocket.QuestionnaireActionDTO where

import GHC.Generics

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Model.User.OnlineUserInfo

data ClientQuestionnaireActionDTO =
  SetContent_ClientQuestionnaireActionDTO
    { _setContent_ClientQuestionnaireActionDTOData :: QuestionnaireEventChangeDTO
    }
  deriving (Show, Generic)

data ServerQuestionnaireActionDTO
  = SetUserList_ServerQuestionnaireActionDTO
      { _setUserList_ServerQuestionnaireActionDTOData :: [OnlineUserInfo]
      }
  | SetContent_ServerQuestionnaireActionDTO
      { _setContent_ServerQuestionnaireActionDTOData :: QuestionnaireEventDTO
      }
  deriving (Show, Eq, Generic)
