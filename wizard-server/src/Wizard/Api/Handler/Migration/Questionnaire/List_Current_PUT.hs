module Wizard.Api.Handler.Migration.Questionnaire.List_Current_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateChangeDTO
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateChangeJM ()
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateDTO
import Wizard.Api.Resource.Questionnaire.Migration.MigratorStateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Questionnaire.Migration.MigrationService

type List_Current_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] MigratorStateChangeDTO
    :> "questionnaires"
    :> Capture "qtnUuid" U.UUID
    :> "migrations"
    :> "current"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] MigratorStateDTO)

list_current_PUT
  :: Maybe String
  -> Maybe String
  -> MigratorStateChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] MigratorStateDTO)
list_current_PUT mTokenHeader mServerUrl reqDto qtnUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyQuestionnaireMigration qtnUuid reqDto
