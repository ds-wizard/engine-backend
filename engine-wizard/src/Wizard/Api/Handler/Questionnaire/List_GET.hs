module Wizard.Api.Handler.Questionnaire.List_GET where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type List_GET
   = Header "Authorization" String
     :> "questionnaires"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] [QuestionnaireDTO])

list_GET :: Maybe String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] [QuestionnaireDTO])
list_GET mTokenHeader =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      checkPermission mTokenHeader "QTN_PERM"
      getQuestionnairesForCurrentUser
