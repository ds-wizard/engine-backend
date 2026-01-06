module Wizard.Api.Handler.Project.File.List_GET where

import qualified Data.UUID as U
import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.File.ProjectFileListJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Model.Project.File.ProjectFileList
import Wizard.Service.Project.File.ProjectFileService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "projects"
    :> Capture "projectUuid" U.UUID
    :> "files"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page ProjectFileList))

list_GET
  :: Maybe String
  -> Maybe String
  -> U.UUID
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page ProjectFileList))
list_GET mTokenHeader mServerUrl uuid mQuery mPage mSize mSort =
  getMaybeAuthServiceExecutor mTokenHeader mServerUrl $ \runInMaybeAuthService ->
    runInMaybeAuthService NoTransaction $
      addTraceUuidHeader =<< getProjectFilesPage mQuery (Just uuid) (Pageable mPage mSize) (parseSortQuery mSort)
