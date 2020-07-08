module Wizard.Api.Handler.Questionnaire.List_POST_CloneUuid where

import Data.Maybe (fromMaybe)
import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type List_POST_CloneUuid
   = Header "Authorization" String
     :> "questionnaires"
     :> QueryParam "cloneUuid" String
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] QuestionnaireDTO)

list_POST_CloneUuid ::
     Maybe String -> Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] QuestionnaireDTO)
list_POST_CloneUuid mTokenHeader mCloneUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< cloneQuestionnaire (fromMaybe "" mCloneUuid)
