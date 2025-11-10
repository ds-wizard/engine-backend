module Wizard.Api.Handler.KnowledgeModelEditor.List_Suggestions_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorSuggestionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Wizard.Service.KnowledgeModel.Editor.EditorService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "knowledge-model-editors"
    :> "suggestions"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page KnowledgeModelEditorSuggestion))

list_suggestions_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page KnowledgeModelEditorSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getEditorSuggestionsPage mQuery (Pageable mPage mSize) (parseSortQuery mSort)
