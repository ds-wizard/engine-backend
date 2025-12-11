module Wizard.Api.Handler.Project.Event.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Event.ProjectEventListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Service.Project.ProjectService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "events"
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page ProjectEventList))

list_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page ProjectEventList))
list_GET mTokenHeader mServerUrl uuid mPage mSize mSort =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader
        =<< getProjectEventsPage uuid (Pageable mPage mSize) (parseSortQuery mSort)
