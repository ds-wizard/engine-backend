module Wizard.Api.Handler.Migration.KnowledgeModel.List_Current_Conflict_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Migration.KnowledgeModel.MigratorService

type List_Current_Conflict_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] MigratorConflictDTO
    :> "branches"
    :> Capture "bUuid" U.UUID
    :> "migrations"
    :> "current"
    :> "conflict"
    :> Verb 'POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_current_conflict_POST
  :: Maybe String
  -> Maybe String
  -> MigratorConflictDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_current_conflict_POST mTokenHeader mServerUrl reqDto bUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        solveConflictAndMigrate bUuid reqDto
        return NoContent
