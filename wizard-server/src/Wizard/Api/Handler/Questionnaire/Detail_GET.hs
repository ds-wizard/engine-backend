module Wizard.Api.Handler.Questionnaire.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireDetailDTO)

detail_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireDetailDTO)
detail_GET mTokenHeader mServerUrl qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getQuestionnaireDetailById qtnUuid
