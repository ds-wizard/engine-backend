module Wizard.Api.Handler.Questionnaire.Detail_History_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireHistoryDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireHistoryJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type Detail_History_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "history"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] QuestionnaireHistoryDTO)

detail_history_GET ::
     Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] QuestionnaireHistoryDTO)
detail_history_GET mTokenHeader mServerUrl qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getQuestionnaireHistoryForQtnUuid qtnUuid
