module Wizard.Api.Handler.Project.Migration.List_Current_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Migration.ProjectMigrationDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.Migration.ProjectMigrationService

type List_Current_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "migrations"
    :> "current"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectMigrationDTO)

list_current_GET
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectMigrationDTO)
list_current_GET mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getProjectMigration uuid
