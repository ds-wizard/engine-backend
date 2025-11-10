module Wizard.Api.Handler.Migration.Questionnaire.List_Current_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Migration.MigrationService

type List_Current_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "migrations"
    :> "current"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] MigratorStateDTO)

list_current_GET
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] MigratorStateDTO)
list_current_GET mTokenHeader mServerUrl qtnUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getQuestionnaireMigration qtnUuid
