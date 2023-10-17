module Wizard.Api.Handler.UserGroup.List_Suggestions_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.User.Group.UserGroupSuggestionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.User.UserGroupSuggestion
import Wizard.Service.User.Group.UserGroupService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "user-groups"
    :> "suggestions"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page UserGroupSuggestion))

list_suggestions_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page UserGroupSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getUserGroupSuggestions mQuery (Pageable mPage mSize) (parseSortQuery mSort)
