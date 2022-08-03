module Wizard.Api.Handler.Questionnaire.Detail_Labels_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelListDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireLabelListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type Detail_Labels_GET
   = Header "Authorization" String
     :> Header "Host" String
     :> "questionnaires"
     :> Capture "qtnUuid" String
     :> "labels"
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] QuestionnaireLabelListDTO)

detail_labels_GET ::
     Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] QuestionnaireLabelListDTO)
detail_labels_GET mTokenHeader mServerUrl qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getQuestionnaireLabelsForQtnUuid qtnUuid
