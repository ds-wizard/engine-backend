module Wizard.Api.Handler.Questionnaire.Event.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "events"
    :> Capture "eventUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireEventDTO)

detail_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireEventDTO)
detail_GET mTokenHeader mServerUrl qtnUuid eventUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getQuestionnaireEventForQtnUuid qtnUuid eventUuid
