module Wizard.Api.Handler.Migration.Questionnaire.List_Current_Completion_POST where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Migration.MigrationService

type List_Current_Completion_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "migrations"
    :> "current"
    :> "completion"
    :> Verb POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_current_completion_POST
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_current_completion_POST mTokenHeader mServerUrl qtnUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        finishQuestionnaireMigration qtnUuid
        return NoContent
