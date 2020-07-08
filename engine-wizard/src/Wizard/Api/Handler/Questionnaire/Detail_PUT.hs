module Wizard.Api.Handler.Questionnaire.Detail_PUT where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireChangeJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type Detail_PUT
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] QuestionnaireChangeDTO
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> Put '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] QuestionnaireDetailDTO)

detail_PUT ::
     Maybe String
  -> QuestionnaireChangeDTO
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] QuestionnaireDetailDTO)
detail_PUT mTokenHeader reqDto qtnUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $ addTraceUuidHeader =<< modifyQuestionnaire qtnUuid reqDto
