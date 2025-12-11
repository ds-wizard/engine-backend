module Wizard.Api.Handler.KnowledgeModelEditor.Migration.List_Current_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Migration.KnowledgeModelMigrationService

type List_Current_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-editors"
    :> Capture "bUuid" U.UUID
    :> "migrations"
    :> "current"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelMigrationDTO)

list_current_GET
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelMigrationDTO)
list_current_GET mTokenHeader mServerUrl bUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getCurrentMigrationDto bUuid
