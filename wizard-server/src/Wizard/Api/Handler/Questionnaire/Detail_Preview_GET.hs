module Wizard.Api.Handler.Questionnaire.Detail_Preview_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailPreviewJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Questionnaire.QuestionnaireDetailPreview
import Wizard.Service.Questionnaire.QuestionnaireService

type Detail_Preview_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "preview"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireDetailPreview)

detail_preview_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireDetailPreview)
detail_preview_GET mTokenHeader mServerUrl qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getQuestionnaireDetailPreviewById qtnUuid
