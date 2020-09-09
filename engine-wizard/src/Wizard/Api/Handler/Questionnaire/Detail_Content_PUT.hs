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
     :> ReqBody '[ SafeJSON] QuestionnaireContentChangeDTO
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "content"
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] QuestionnaireContentChangeDTO)

detail_content_PUT ::
     Maybe String
  -> QuestionnaireContentChangeDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] QuestionnaireContentChangeDTO)
detail_content_PUT mTokenHeader reqDto qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< modifyContent qtnUuid reqDto
