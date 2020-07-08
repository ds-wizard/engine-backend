module Wizard.Api.Handler.Migration.KnowledgeModel.List_Current_Conflict_POST where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorConflictJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Migration.KnowledgeModel.MigratorService

type List_Current_Conflict_POST
   = Header "Authorization" String
     :> ReqBody '[ SafeJSON] MigratorConflictDTO
     :> "branches"
     :> Capture "bUuid" String
     :> "migrations"
     :> "current"
     :> "conflict"
     :> Verb 'POST 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

list_current_conflict_POST ::
     Maybe String -> MigratorConflictDTO -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
list_current_conflict_POST mTokenHeader reqDto bUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      solveConflictAndMigrate bUuid reqDto
      return NoContent
