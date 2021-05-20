module Wizard.Api.Handler.Questionnaire.List_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type List_POST
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] QuestionnaireCreateDTO
     :> "questionnaires"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] QuestionnaireDTO)

list_POST ::
     Maybe String -> QuestionnaireCreateDTO -> BaseContextM (Headers '[ Header "x-trace-uuid" String] QuestionnaireDTO)
list_POST mTokenHeader reqDto =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInMaybeAuthService ->
    runInMaybeAuthService $ addTraceUuidHeader =<< createQuestionnaire reqDto
