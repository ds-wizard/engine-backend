module Wizard.Api.Handler.Migration.KnowledgeModel.List_Current_DELETE where

import Servant

import Shared.Api.Handler.Common
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Migration.KnowledgeModel.MigratorService

type List_Current_DELETE
   = Header "Authorization" String
     :> "branches"
     :> Capture "bUuid" String
     :> "migrations"
     :> "current"
     :> Verb DELETE 204 '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] NoContent)

list_current_DELETE :: Maybe String -> String -> BaseContextM (Headers '[ Header "x-trace-uuid" String] NoContent)
list_current_DELETE mTokenHeader bUuid =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< do
      deleteCurrentMigration bUuid
      return NoContent
