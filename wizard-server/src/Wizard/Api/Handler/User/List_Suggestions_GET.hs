module Wizard.Api.Handler.User.List_Suggestions_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Shared.Common.Util.String (splitOn)
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.User.UserService
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()
import WizardLib.Public.Model.User.UserSuggestion

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "users"
    :> "suggestions"
    :> QueryParam "q" String
    :> QueryParam "select" String
    :> QueryParam "exclude" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page UserSuggestion))

list_suggestions_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page UserSuggestion))
list_suggestions_GET mTokenHeader mServerUrl mQuery mSelect mExclude mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< do
        let mSelectUuids = fmap (splitOn ",") mSelect
        let mExcludeUuids = fmap (splitOn ",") mExclude
        getUserSuggestionsPage mQuery mSelectUuids mExcludeUuids (Pageable mPage mSize) (parseSortQuery mSort)
