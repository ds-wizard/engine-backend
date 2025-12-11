module Wizard.Api.Handler.ProjectFile.List_GET where

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
    :> "project-files"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page ProjectFileList))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page ProjectFileList))
list_GET mTokenHeader mServerUrl mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getProjectFilesPage mQuery Nothing (Pageable mPage mSize) (parseSortQuery mSort)
