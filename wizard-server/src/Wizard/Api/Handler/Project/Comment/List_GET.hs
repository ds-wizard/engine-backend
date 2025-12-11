module Wizard.Api.Handler.Project.Comment.List_GET where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Comment.ProjectCommentThreadListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Project.Comment.ProjectCommentList
import Wizard.Service.Project.Comment.ProjectCommentService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> QueryParam "path" String
    :> QueryParam "resolved" Bool
    :> "comments"
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (M.Map String [ProjectCommentThreadList]))

list_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe String
  -> Maybe Bool
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (M.Map String [ProjectCommentThreadList]))
list_GET mTokenHeader mServerUrl uuid mPath mResolved =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $ addTraceUuidHeader =<< getProjectCommentsByProjectUuid uuid mPath mResolved
