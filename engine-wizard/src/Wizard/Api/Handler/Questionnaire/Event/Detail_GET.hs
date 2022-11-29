module Wizard.Api.Handler.Questionnaire.Event.Detail_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" String
    :> "events"
    :> Capture "eventUuid" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireEventDTO)

detail_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireEventDTO)
detail_GET mTokenHeader mServerUrl qtnUuid eventUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getQuestionnaireEventForQtnUuid qtnUuid eventUuid
