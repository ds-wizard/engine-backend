module Wizard.Api.Handler.ProjectImporter.Detail_PUT where

import Servant

import Shared.Common.Api.Handler.Common
import Shared.Common.Model.Context.TransactionState
import Wizard.Api.Handler.Common
import Wizard.Api.Resource.Project.Importer.ProjectImporterChangeDTO
import Wizard.Api.Resource.Project.Importer.ProjectImporterChangeJM ()
import Wizard.Api.Resource.Project.Importer.ProjectImporterDTO
import Wizard.Api.Resource.Project.Importer.ProjectImporterJM ()
import Wizard.Model.Context.BaseContext
import Wizard.Service.Project.Importer.ProjectImporterService

type Detail_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectImporterChangeDTO
    :> "project-importers"
    :> Capture "piId" String
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectImporterDTO)

detail_PUT
  :: Maybe String
  -> Maybe String
  -> ProjectImporterChangeDTO
  -> String
  -> BaseContextM (Headers '[Header "x-trace-uuid" String] ProjectImporterDTO)
detail_PUT mTokenHeader mServerUrl reqDto piId =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyProjectImporter piId reqDto
