module Wizard.Api.Resource.Websocket.QuestionnaireActionDTO where

import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireEventDTO
import Wizard.Model.User.OnlineUserInfo

data ClientQuestionnaireActionDTO
  = SetReply_ClientQuestionnaireActionDTO
      { _setReply_ClientQuestionnaireActionDTOData :: SetReplyEventDTO
      }
  | ClearReply_ClientQuestionnaireActionDTO
      { _clearReply_ClientQuestionnaireActionDTOData :: ClearReplyEventDTO
      }
  | SetLevel_ClientQuestionnaireActionDTO
      { _setLevel_ClientQuestionnaireActionDTOData :: SetLevelEventDTO
      }
  | SetLabels_ClientQuestionnaireActionDTO
      { _setLabels_ClientQuestionnaireActionDTOData :: SetLabelsEventDTO
      }
  deriving (Show, Generic)

data ServerQuestionnaireActionDTO
  = SetUserList_ServerQuestionnaireActionDTO
      { _setUserList_ServerQuestionnaireActionDTOData :: [OnlineUserInfo]
      }
  | SetReply_ServerQuestionnaireActionDTO
      { _setReply_ServerQuestionnaireActionDTOData :: SetReplyEventDTO
      }
  | ClearReply_ServerQuestionnaireActionDTO
      { _clearReply_ServerQuestionnaireActionDTOData :: ClearReplyEventDTO
      }
  | SetLevel_ServerQuestionnaireActionDTO
      { _setLevel_ServerQuestionnaireActionDTOData :: SetLevelEventDTO
      }
  | SetLabels_ServerQuestionnaireActionDTO
      { _setLabels_ServerQuestionnaireActionDTOData :: SetLabelsEventDTO
      }
  deriving (Show, Eq, Generic)
