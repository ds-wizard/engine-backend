module Wizard.Api.Handler.Project.Migration.List_POST where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateJM ()
import Wizard.Api.Resource.Project.Migration.ProjectMigrationDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.Migration.ProjectMigrationService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectMigrationCreateDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "migrations"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectMigrationDTO)

list_POST
  :: Maybe String
  -> Maybe String
  -> ProjectMigrationCreateDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectMigrationDTO)
list_POST mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createProjectMigration uuid reqDto
