module Wizard.Api.Handler.ProjectAction.List_Suggestions_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Action.ProjectActionDTO
import Wizard.Api.Resource.Project.Action.ProjectActionJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.Action.ProjectActionService

type List_Suggestions_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "project-actions"
    :> "suggestions"
    :> QueryParam "projectUuid" U.UUID
    :> QueryParam "q" String
    :> QueryParam "enabled" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page ProjectActionDTO))

list_suggestions_GET
  :: Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> Maybe String
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page ProjectActionDTO))
list_suggestions_GET mTokenHeader mServerUrl mProjectUuid mQuery mEnabled mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getProjectActionSuggestions mProjectUuid mQuery mEnabled (Pageable mPage mSize) (parseSortQuery mSort)
