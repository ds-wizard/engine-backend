module Wizard.Api.Handler.Branch.List_Suggestions_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Branch.BranchSuggestionJM ()
import Wizard.Model.Branch.BranchSuggestion
import Wizard.Model.Context.BaseContext
import Wizard.Service.Branch.BranchService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "branches"
    :> "suggestions"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page BranchSuggestion))

list_suggestions_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page BranchSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getBranchSuggestionsPage mQuery (Pageable mPage mSize) (parseSortQuery mSort)
