module Wizard.Api.Handler.Locale.List_Suggestions_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Shared.Locale.Api.Resource.Locale.LocaleSuggestionJM ()
import Shared.Locale.Model.Locale.LocaleSuggestion
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Locale.LocaleService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "locales"
    :> "suggestions"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page LocaleSuggestion))

list_suggestions_GET :: Maybe String -> Maybe String -> Maybe String -> Maybe Int -> Maybe Int -> Maybe String -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page LocaleSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getLocaleSuggestions mQuery (Pageable mPage mSize) (parseSortQuery mSort)
