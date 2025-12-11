module Wizard.Api.Handler.Project.Migration.List_Current_Completion_POST where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.Migration.ProjectMigrationService

type List_Current_Completion_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "migrations"
    :> "current"
    :> "completion"
    :> Verb POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_current_completion_POST
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_current_completion_POST mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        finishProjectMigration uuid
        return NoContent
