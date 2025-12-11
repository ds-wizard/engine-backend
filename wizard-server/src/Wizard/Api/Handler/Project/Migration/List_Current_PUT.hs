module Wizard.Api.Handler.Project.Migration.List_Current_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Migration.ProjectMigrationChangeDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationChangeJM ()
import Wizard.Api.Resource.Project.Migration.ProjectMigrationDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.Migration.ProjectMigrationService

type List_Current_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectMigrationChangeDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "migrations"
    :> "current"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectMigrationDTO)

list_current_PUT
  :: Maybe String
  -> Maybe String
  -> ProjectMigrationChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectMigrationDTO)
list_current_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyProjectMigration uuid reqDto
