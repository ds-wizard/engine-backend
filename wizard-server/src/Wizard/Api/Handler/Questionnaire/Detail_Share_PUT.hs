module Wizard.Api.Handler.Questionnaire.Detail_Share_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireShareChangeDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireShareChangeJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.QuestionnaireService

type Detail_Share_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] QuestionnaireShareChangeDTO
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "share"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireShareChangeDTO)

detail_share_PUT
  :: Maybe String
  -> Maybe String
  -> QuestionnaireShareChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireShareChangeDTO)
detail_share_PUT mTokenHeader mServerUrl reqDto qtnUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyQuestionnaireShare qtnUuid reqDto
