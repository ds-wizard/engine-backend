module Wizard.Api.Handler.Migration.KnowledgeModel.List_Current_Conflict_All_POST where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Migration.KnowledgeModel.MigratorService

type List_Current_Conflict_All_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> "branches"
    :> Capture "bUuid" U.UUID
    :> "migrations"
    :> "current"
    :> "conflict"
    :> "all"
    :> Verb 'POST 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_Current_Conflict_All_POST
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_Current_Conflict_All_POST mTokenHeader mServerUrl bUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        solveAllConflicts bUuid
        return NoContent
