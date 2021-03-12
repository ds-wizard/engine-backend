module Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireActions where

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventChangeDTO
import Wizard.Api.Resource.Websocket.QuestionnaireActionDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

ensureOnlineUserAction :: ClientQuestionnaireActionDTO
ensureOnlineUserAction =
  SetContent_ClientQuestionnaireActionDTO . SetReplyEventChangeDTO' $ toSetReplyEventChangeDTO sre_rQ1

setUserListAction :: ServerQuestionnaireActionDTO
setUserListAction = SetUserList_ServerQuestionnaireActionDTO [userAlbertOnlineInfo]
