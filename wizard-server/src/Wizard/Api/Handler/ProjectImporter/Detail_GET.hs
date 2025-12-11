module Wizard.Api.Handler.ProjectImporter.Detail_GET where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Importer.ProjectImporterDTO
import Wizard.Api.Resource.Project.Importer.ProjectImporterJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.Importer.ProjectImporterService

type Detail_GET =
  Header "Authorization" String
    :> Header "Host" String
    :> "project-importers"
    :> Capture "piId" String
    :> Get '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectImporterDTO)

detail_GET
  :: Maybe String
  -> Maybe String
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectImporterDTO)
detail_GET mTokenHeader mServerUrl piId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< getProjectImporter piId
