module Wizard.Api.Handler.Questionnaire.Version.List_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionService

type List_POST
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] QuestionnaireVersionChangeDTO
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "versions"
     :> Verb 'POST 201 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] QuestionnaireVersionDTO)

list_POST ::
     Maybe String
  -> Maybe String
  -> QuestionnaireVersionChangeDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] QuestionnaireVersionDTO)
list_POST mTokenHeader mServerUrl reqDto qtnUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< createVersion qtnUuid reqDto
