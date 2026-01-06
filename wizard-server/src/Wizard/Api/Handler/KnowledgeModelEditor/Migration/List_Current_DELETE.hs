module Wizard.Api.Handler.KnowledgeModelEditor.Migration.List_Current_DELETE where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Migration.KnowledgeModelMigrationService

type List_Current_DELETE =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-editors"
    :> Capture "bUuid" U.UUID
    :> "migrations"
    :> "current"
    :> Verb DELETE 204 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] NoContent)

list_current_DELETE
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] NoContent)
list_current_DELETE mTokenHeader mServerUrl bUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $
      addTraceUuidHeader =<< do
        deleteCurrentMigration bUuid
        return NoContent
