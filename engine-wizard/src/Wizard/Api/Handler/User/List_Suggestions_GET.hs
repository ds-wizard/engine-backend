module Wizard.Api.Handler.User.List_Suggestions_GET where

import Servant

import Shared.Api.Handler.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Api.Resource.User.UserSuggestionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService

type List_Suggestions_GET
   = Header "Authorization" String
     :> "users"
     :> "suggestions"
     :> QueryParam "q" String
     :> QueryParam "page" Int
     :> QueryParam "size" Int
     :> QueryParam "sort" String
     :> Get '[ SafeJSON] (Headers '[ Header "x-trace-uuid" String] (Page UserSuggestionDTO))

list_suggestions_GET ::
     Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[ Header "x-trace-uuid" String] (Page UserSuggestionDTO))
list_suggestions_GET mTokenHeader mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader $ \runInAuthService ->
    runInAuthService $
    addTraceUuidHeader =<< getUserSuggestionsPage mQuery (Pageable mPage mSize) (parseSortQuery mSort)
