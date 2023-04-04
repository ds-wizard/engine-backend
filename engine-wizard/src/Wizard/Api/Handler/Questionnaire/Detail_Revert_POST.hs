module Wizard.Api.Handler.Questionnaire.Detail_Revert_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentJM ()
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionService

type Detail_Revert_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] QuestionnaireVersionRevertDTO
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "revert"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireContentDTO)

detail_revert_POST
  :: Maybe String
  -> Maybe String
  -> QuestionnaireVersionRevertDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireContentDTO)
detail_revert_POST mTokenHeader mServerUrl reqDto qtnUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< revertToEvent qtnUuid reqDto True
