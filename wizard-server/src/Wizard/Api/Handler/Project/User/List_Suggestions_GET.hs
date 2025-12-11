module Wizard.Api.Handler.Project.User.List_Suggestions_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.User.ProjectUserService
import WizardLib.Public.Api.Resource.User.UserSuggestionJM ()
import WizardLib.Public.Model.User.UserSuggestion

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "users"
    :> "suggestions"
    :> QueryParam "q" String
    :> QueryParam "editor" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page UserSuggestion))

list_suggestions_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe String
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page UserSuggestion))
list_suggestions_GET mTokenHeader mServerUrl uuid mQuery mEditor mPage mSize mSort =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $
      addTraceUuidHeader =<< getProjectUserSuggestionsPage uuid mQuery mEditor (Pageable mPage mSize) (parseSortQuery mSort)
