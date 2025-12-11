module Wizard.Api.Handler.KnowledgeModelEditor.Migration.List_Current_POST where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateJM ()
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationStateJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Migration.KnowledgeModelMigrationService

type List_Current_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelMigrationCreateDTO
    :> "knowledge-model-editors"
    :> Capture "bUuid" U.UUID
    :> "migrations"
    :> "current"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelMigrationDTO)

list_current_POST
  :: Maybe String
  -> Maybe String
  -> KnowledgeModelMigrationCreateDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelMigrationDTO)
list_current_POST mTokenHeader mServerUrl reqDto bUuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createMigration bUuid reqDto
