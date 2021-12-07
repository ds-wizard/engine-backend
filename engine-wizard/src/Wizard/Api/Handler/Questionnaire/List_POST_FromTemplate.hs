module Wizard.Api.Handler.Questionnaire.List_POST_FromTemplate where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type List_POST_FromTemplate
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] QuestionnaireCreateFromTemplateDTO
     :> "questionnaires"
     :> "from-template"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] QuestionnaireDTO)

list_POST_FromTemplate ::
     Maybe String
  -> Maybe String
  -> QuestionnaireCreateFromTemplateDTO
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] QuestionnaireDTO)
list_POST_FromTemplate mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< createQuestionnaireFromTemplate reqDto
