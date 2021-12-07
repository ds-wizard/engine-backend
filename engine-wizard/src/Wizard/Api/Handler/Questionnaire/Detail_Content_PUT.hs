module Wizard.Api.Handler.Questionnaire.Detail_Content_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type Detail_Content_PUT
   = Header "Authorization" String
     :> Header "Host" String
     :> ReqBody '[ SafeJSON] QuestionnaireContentChangeDTO
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "content"
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] QuestionnaireContentChangeDTO)

detail_content_PUT ::
     Maybe String
  -> Maybe String
  -> QuestionnaireContentChangeDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] QuestionnaireContentChangeDTO)
detail_content_PUT mTokenHeader mServerUrl reqDto qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< modifyContent qtnUuid reqDto
