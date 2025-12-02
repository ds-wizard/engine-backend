module Wizard.Api.Handler.KnowledgeModelEditor.Detail_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeJM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.KnowledgeModel.Editor.EditorService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelEditorChangeDTO
    :> "knowledge-model-editors"
    :> Capture "uuid" U.UUID
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorDetailDTO)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> KnowledgeModelEditorChangeDTO
  -> U.UUID
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorDetailDTO)
detail_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyEditor uuid reqDto
