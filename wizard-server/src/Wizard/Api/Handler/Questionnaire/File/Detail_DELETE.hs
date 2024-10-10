module Wizard.Api.Handler.Questionnaire.File.Detail_DELETE where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.File.QuestionnaireFileService

type Detail_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "questionnaireUuid" U.UUID
    :> "files"
    :> Capture "fileUuid" U.UUID
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

detail_DELETE :: Maybe String -> Maybe String -> U.UUID -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
detail_DELETE mTokenHeader mServerUrl qtnUuid fileUuid =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteQuestionnaireFile qtnUuid fileUuid
        return NoContent
