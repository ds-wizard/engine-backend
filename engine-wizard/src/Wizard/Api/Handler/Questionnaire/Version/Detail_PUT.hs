module Wizard.Api.Handler.Questionnaire.Version.Detail_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionChangeJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionService

type Detail_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] QuestionnaireVersionChangeDTO
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "versions"
     :> Capture "vUuid" String
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] QuestionnaireVersionDTO)

detail_PUT ::
     Maybe String
  -> QuestionnaireVersionChangeDTO
  -> String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] QuestionnaireVersionDTO)
detail_PUT mTokenHeader reqDto qtnUuid vUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< modifyVersion qtnUuid vUuid reqDto
