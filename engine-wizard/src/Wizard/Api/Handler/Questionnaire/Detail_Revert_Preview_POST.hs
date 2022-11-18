module Wizard.Api.Handler.Questionnaire.Detail_Revert_Preview_POST where

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

type Detail_Revert_Preview_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] QuestionnaireVersionRevertDTO
    :> "questionnaires"
    :> Capture "qtnUuid" String
    :> "revert"
    :> "preview"
    :> Post '[SafeJSON] (Headers '[Header "x-trace-uuid" String] QuestionnaireContentDTO)

detail_revert_preview_POST
  :: Maybe String
  -> Maybe String
  -> QuestionnaireVersionRevertDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] QuestionnaireContentDTO)
detail_revert_preview_POST mTokenHeader mServerUrl reqDto qtnUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< revertToEvent qtnUuid reqDto False
