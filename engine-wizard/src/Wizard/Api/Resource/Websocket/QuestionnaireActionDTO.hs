module Wizard.Api.Resource.Websocket.QuestionnaireActionDTO where

import GHC.Generics

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailWsDTO
import Wizard.Model.User.OnlineUserInfo

data ClientQuestionnaireActionDTO = SetContent_ClientQuestionnaireActionDTO
  { aData :: QuestionnaireEventChangeDTO
  }
  deriving (Show, Generic)

data ServerQuestionnaireActionDTO
  = SetUserList_ServerQuestionnaireActionDTO
      { ouiData :: [OnlineUserInfo]
      }
  | SetContent_ServerQuestionnaireActionDTO
      { qeData :: QuestionnaireEventDTO
      }
  | SetQuestionnaire_ServerQuestionnaireActionDTO
      { sqData :: QuestionnaireDetailWsDTO
      }
  deriving (Show, Eq, Generic)
