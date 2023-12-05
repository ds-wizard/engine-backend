module Wizard.Api.Handler.QuestionnaireAction.Detail_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionDTO
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.QuestionnaireAction.QuestionnaireActionService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaire-actions"
    :> Capture "id" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireActionDTO)

detail_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireActionDTO)
detail_GET mTokenHeader mServerUrl qaId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getQuestionnaireAction qaId
