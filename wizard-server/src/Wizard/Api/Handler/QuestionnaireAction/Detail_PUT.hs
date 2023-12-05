module Wizard.Api.Handler.QuestionnaireAction.Detail_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionChangeDTO
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionChangeJM ()
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionDTO
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.QuestionnaireAction.QuestionnaireActionService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] QuestionnaireActionChangeDTO
    :> "questionnaire-actions"
    :> Capture "id" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireActionDTO)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> QuestionnaireActionChangeDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireActionDTO)
detail_PUT mTokenHeader mServerUrl reqDto qaId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyQuestionnaireAction qaId reqDto
