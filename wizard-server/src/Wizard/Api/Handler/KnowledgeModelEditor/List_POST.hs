module Wizard.Api.Handler.KnowledgeModelEditor.List_POST where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateJM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Wizard.Service.KnowledgeModel.Editor.EditorService

type List_POST =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] KnowledgeModelEditorCreateDTO
    :> "knowledge-model-editors"
    :> Verb 'POST 201 '[SafeJSON] (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorList)

list_POST
  :: Maybe String
  -> Maybe String
  -> KnowledgeModelEditorCreateDTO
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] KnowledgeModelEditorList)
list_POST mTokenHeader mServerUrl reqDto =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< createEditor reqDto
