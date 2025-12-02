module Wizard.Api.Handler.KnowledgeModelEditor.Detail_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Editor.EditorService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-editors"
    :> Capture "uuid" U.UUID
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorDetailDTO)

detail_GET
  :: Maybe String -> Maybe String -> U.UUID -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorDetailDTO)
detail_GET mTokenHeader mServerUrl uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getEditorByUuid uuid
