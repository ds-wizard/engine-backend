module Wizard.Api.Handler.ProjectCommentThread.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Comment.ProjectCommentThreadAssignedJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Project.Comment.ProjectCommentThreadAssigned
import Wizard.Service.Project.Comment.ProjectCommentService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "project-comment-threads"
    :> QueryParam "q" String
    :> QueryParam "projectUuid" U.UUID
    :> QueryParam "resolved" Bool
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page ProjectCommentThreadAssigned))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe U.UUID
  -> Maybe Bool
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page ProjectCommentThreadAssigned))
list_GET mTokenHeader mServerUrl mQuery mProjectUuid resolved mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getProjectCommentThreadsPage mQuery mProjectUuid resolved (Pageable mPage mSize) (parseSortQuery mSort)
