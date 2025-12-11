module Wizard.Api.Handler.ProjectImporter.List_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Importer.ProjectImporterDTO
import Wizard.Api.Resource.Project.Importer.ProjectImporterJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.Importer.ProjectImporterService

type List_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "project-importers"
    :> QueryParam "q" String
    :> QueryParam "page" Int
    :> QueryParam "size" Int
    :> QueryParam "sort" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] (Page ProjectImporterDTO))

list_GET
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Int
  -> Maybe Int
  -> Maybe String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] (Page ProjectImporterDTO))
list_GET mTokenHeader mServerUrl mQuery mPage mSize mSort =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService NoTransaction $
      addTraceUuidHeader =<< getProjectImportersPageDto mQuery (Pageable mPage mSize) (parseSortQuery mSort)
