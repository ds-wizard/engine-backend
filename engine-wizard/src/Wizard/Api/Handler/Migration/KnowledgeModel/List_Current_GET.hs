module Wizard.Api.Handler.Migration.KnowledgeModel.List_Current_GET where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Migration.KnowledgeModel.MigratorService

type List_Current_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "branches"
    :> Capture "bUuid" U.UUID
    :> "migrations"
    :> "current"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] MigratorStateDTO)

list_current_GET
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] MigratorStateDTO)
list_current_GET mTokenHeader mServerUrl bUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getCurrentMigrationDto bUuid
